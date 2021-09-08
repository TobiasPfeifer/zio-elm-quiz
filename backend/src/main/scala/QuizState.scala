import Model._
import zio.stm.{STM, TMap, TaskSTM}
import zio.{Has, URLayer, ZEnv}

import java.time.Instant
import java.util.UUID
import scala.util.Random
import zio.optics.toptics._

trait QuizState {
  def getCurrentQuestion(gameId: GameId): TaskSTM[QuestionWithSolution]

  def getGameSetup(gameId: GameId): TaskSTM[GameSetup]

  def getRunningGame(gameId: GameId): TaskSTM[RunningGame]

  def getGameId(gamePin: GamePin): TaskSTM[GameId]

  def getQuizzes(limit: Int): TaskSTM[List[QuizEntity]]

  def getQuiz(quizId: QuizId): TaskSTM[QuizEntity]

  def createQuiz(quiz: Quiz): TaskSTM[QuizEntity]

  def advanceGame(gameId: GameId): TaskSTM[Unit]

  def waitUntilAllPlayersAnswered(gameId: GameId): TaskSTM[Unit]

  def getAndDelete(gameId: GameId): TaskSTM[RunningGame]

  def startGame(gameId: GameId, adminCode: AdminCode): TaskSTM[RunningGame]

  def playerJoins(gameId: GameId, player: Player): TaskSTM[Unit]

  def answer(playerCode: Model.PlayerCode, gameId: GameId, answer: Answer): TaskSTM[ Unit]

  def createGame(title: String, quizId: QuizId, eventQueue: EventQueue[Model.Event]): TaskSTM[GameSetup]

}

object QuizState {
  val live: URLayer[ZEnv, Has[QuizState]] = QuizStateLive.layer
}

case class QuizStateLive(
  quizzes: TMap[QuizId, QuizEntity],
  games: TMap[GameId, RunningGame],
  gamesInSetup: TMap[GameId, GameSetup],
  pinToGameId: TMap[GamePin, GameId]
) extends QuizState { self =>

  override def createGame(title: String, quizId: QuizId, eventQueue: EventQueue[Model.Event]): TaskSTM[GameSetup] = {
    for {
      quiz      <- quizzes.key(quizId).get(())
      adminCode = AdminCode(UUID.randomUUID().toString)
      gamePin   = GamePin(Random.alphanumeric.take(6).mkString(""))
      gameId    = GameId(UUID.randomUUID().toString)
      game      = Game(gameId, title, gamePin, adminCode, quiz, Instant.now().getEpochSecond)
      gameState = GameSetup(game, Map.empty, eventQueue)
      _        <- gamesInSetup.put(gameId, gameState)
      _        <- pinToGameId.put(gamePin, gameId)
    } yield gameState
  }


  override def answer(playerCode: Model.PlayerCode, gameId: GameId, answer: Answer): TaskSTM[Unit] = {
    val gameOptional = self.games.key(gameId)
    val currentAnswer = RunningGame.Optics.currentPlayerAnswer(playerCode)
    val answerOptional: Optional[Any, Answer] = gameOptional.andThen(currentAnswer)

    answerOptional.set(answer).unit
  }

  override def playerJoins(gameId: GameId, player: Player): TaskSTM[Unit] = {
    self.gamesInSetup.key(gameId).update(_.addPlayer(player)).unit
  }

  override def startGame(gameId: GameId, adminCode: AdminCode): TaskSTM[RunningGame] = {
    for {
      game <- self.gamesInSetup.key(gameId).get(())
      _ <- self.gamesInSetup.delete(gameId)
      _ <- self.pinToGameId.removeIf((_, id: Model.GameId) => id == gameId)
      runningGame = RunningGame.start(game)
      _ <- self.games.put(gameId, runningGame)
    } yield runningGame
  }

  override def getAndDelete(gameId: GameId): TaskSTM[RunningGame] = {
    for {
      gameState <- self.games.key(gameId).get(())
      _ <- games.delete(gameId)
    } yield gameState
  }

  override def createQuiz(quiz: Quiz): TaskSTM[QuizEntity] = {
    val quizId     = QuizId(UUID.randomUUID().toString)
    val quizEntity = QuizEntity(quizId, quiz)
    for {
     _ <- quizzes.put(quizId, quizEntity)
    } yield quizEntity
  }

  override def getQuiz(quizId: QuizId): TaskSTM[QuizEntity] = {
    quizzes.key(quizId).get(())
  }

  override def getQuizzes(limit: Int): TaskSTM[List[QuizEntity]] = {
    quizzes.values.map(list => list.take(Math.min(limit, list.size)))
  }



  override def advanceGame(gameId: GameId): TaskSTM[Unit] = {
    self.games.key(gameId).update(_.advance()).unit
  }

  override def waitUntilAllPlayersAnswered(gameId: GameId): TaskSTM[Unit] = {
    for {
      gameState <- games.key(gameId).get(())
      index = gameState.questionIndex
      playerVoteMissing = gameState.players.values.map(_.hasAnswered(index)).exists(!_)
      _ <- STM.retry.when(playerVoteMissing)
    } yield ()
  }

  override def getRunningGame(gameId: GameId): TaskSTM[RunningGame] =
    self.games.key(gameId).get(())

  override def getGameId(gamePin: GamePin): TaskSTM[GameId] =
    self.pinToGameId.key(gamePin).get(())

  override def getGameSetup(gameId: GameId): TaskSTM[GameSetup] =
    self.gamesInSetup.key(gameId).get(())

  override def getCurrentQuestion(gameId: GameId): TaskSTM[QuestionWithSolution] = {
    val gameOptional = self.games.key(gameId)
    val currentQuestion: Optional[RunningGame, QuestionWithSolution] = RunningGame.Optics.currentQuestion
    val combined: Optional[Any, QuestionWithSolution] = gameOptional >>> currentQuestion
    combined.get(())
  }
}

object QuizStateLive {
  val quizStateLive = STM.atomically {
    for {
      quizzes <- TMap.empty[QuizId, QuizEntity]
      games <- TMap.empty[GameId, RunningGame]
      gamesInSetup <- TMap.empty[GameId, GameSetup]
      pinToGameId <- TMap.empty[GamePin, GameId]
      quizStateLive = QuizStateLive(quizzes, games, gamesInSetup, pinToGameId)
    } yield quizStateLive
  }

  val layer: URLayer[ZEnv, Has[QuizState]] = {
    quizStateLive.toLayer
  }
}
