import Model.Event._
import Model._
import zio.console.putStrLn
import zio.stream.ZStream
import zio.{Has, IO, RIO, Task, UIO, URLayer, ZEnv, ZIO}

import scala.language.postfixOps

trait QuizApp {
  def answer(playerCode: PlayerCode, gameId: GameId, answer: Answer): RIO[ZEnv, Unit]

  def joinGame(gamePin: GamePin, playerName: String): ZStream[ZEnv, Throwable, Event]

  def startGame(adminCode: AdminCode, gameId: GameId): RIO[ZEnv, Unit]

  def createGame(title: String, quizId: Model.QuizId): RIO[ZEnv, Game]

  def create(quiz: Model.Quiz): RIO[ZEnv, QuizEntity]

  def getQuiz(quizId: Model.QuizId): RIO[ZEnv, Option[Model.QuizEntity]]

  def getQuizzes(limit: Int): RIO[ZEnv, List[Model.QuizEntity]]
}

object QuizApp {
  val live: URLayer[ZEnv with Has[QuizState], Has[QuizApp]] = QuizAppLive.layer
}

case class QuizAppLive(
                        state: QuizState
) extends QuizApp {

  override def createGame(title: String, quizId: Model.QuizId): Task[Game] =
    for {
      eventQueue <- EventQueue.make[Event]
      gameState <- state.createGame(title, quizId, eventQueue).commit
    } yield gameState.game

  override def answer(playerCode: PlayerCode, gameId: GameId, answer: Answer): Task[Unit] = {
    state.answer(playerCode, gameId, answer).commit
  }

  override def joinGame(gamePin: GamePin, playerName: String): ZStream[ZEnv, Throwable, Event] = {
    val player = Player.create(playerName)
    val stateUpdate = for {
      gameId <- state.getGameId(gamePin)
      _ <- state.playerJoins(gameId, player)
      gameState <- state.getGameSetup(gameId)
      eventStream  = gameState.eventQueue.stream()
      privateEvent = PlayerCodeCreated(player.playerId, player.playerCode, gameState.game.gameId)
      publicEvent  = PlayerJoined(player.playerId, playerName, gameState.players.map(p =>  (p._2.playerId, p._2.name)))
      _           <- gameState.eventQueue.offer(publicEvent)
      stream  = ZStream(privateEvent) ++ ZStream(publicEvent) ++ eventStream
    } yield stream

    ZStream.fromEffect(stateUpdate.commit).flatten
  }

  override def startGame(adminCode: AdminCode, gameId: GameId): RIO[ZEnv, Unit] = {
    for {
      gameState <- state.startGame(gameId, adminCode).commit
      _         <- gameState.eventQueue.offer(GameStarted(gameId)).commit
      shutdown   = putStrLn("shutting eventQueue down") *> gameState.eventQueue.shutdown()
      _         <- (GameLoop.run(gameId, state) *> shutdown).forkDaemon
    } yield ()
  }

  override def create(quiz: Quiz): Task[QuizEntity] =
    state.createQuiz(quiz).commit

  override def getQuiz(quizId: QuizId): Task[Option[QuizEntity]] =
    state.getQuiz(quizId).option.commit

  override def getQuizzes(limit: Int): Task[List[QuizEntity]] =
    state.getQuizzes(limit).commit

}

object QuizAppLive {
  val layer: URLayer[ZEnv with Has[QuizState], Has[QuizApp]] =
    ZIO.service[QuizState].map(QuizAppLive(_)).toLayer
}
