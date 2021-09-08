import Model.Event.{GameFinished, NextQuestion, QuestionEnded}
import Model.{Event, GameId, QuestionWithSolution, RunningGame}
import zio.clock.Clock
import zio.console.putStrLn
import zio.duration.durationInt
import zio.stm.{STM, TaskSTM}
import zio.{RIO, UIO, URIO, ZEnv, ZIO}

object GameLoop {
  def run(gameId: GameId, quizState: QuizState): RIO[ZEnv, Unit] = {
    val finishGame: TaskSTM[Unit] = {
      for {
        gameState <- quizState.getAndDelete(gameId)
        _         <- gameState.eventQueue.offer(GameFinished(gameState.game, buildLeaderBoard(gameState)))
      } yield ()
    }

    lazy val loop: RIO[ZEnv, Unit] = for {
      shouldRepeat <- singleGameStep(gameId, quizState)
      _            <- ZIO.when(shouldRepeat)(loop)
    } yield ()

    loop *> finishGame.commit *> putStrLn("game finished")
  }

  def singleGameStep(gameId: GameId, quizState: QuizState): RIO[ZEnv, Boolean] = {
    val advanceGame = {
      for {
        lastQuestion  <- quizState.getCurrentQuestion(gameId)
        _             <- quizState.advanceGame(gameId)
        gameState     <- quizState.getRunningGame(gameId)
        leaderboard   = buildLeaderBoard(gameState)
        _             <- gameState.eventQueue.offer(QuestionEnded(lastQuestion, leaderboard))
      } yield ()
    }.commit

    val waitForQuestionClosed = quizState.waitUntilAllPlayersAnswered(gameId).commit.timeout(30.seconds)

    for {
      game <- quizState.getRunningGame(gameId).commit
      currentQuestion = game.getCurrentQuestion()
      shouldRepeat <- currentQuestion match {
        case Some(question) => for {
          _ <- game.eventQueue.offer(NextQuestion(question.withoutSolution())).commit
          _ <- waitForQuestionClosed
          _ <- advanceGame
          _ <- ZIO.sleep(3.seconds)
        } yield true
        case None => ZIO.succeed(false)
      }
    } yield shouldRepeat
  }

  def buildLeaderBoard(gameState: RunningGame): List[Event.LeaderboardEntry] =
    gameState.players.values
      .map(p => Event.LeaderboardEntry(p.playerId, p.name, p.score))
      .toList
      .sortBy(- _.score)
}
