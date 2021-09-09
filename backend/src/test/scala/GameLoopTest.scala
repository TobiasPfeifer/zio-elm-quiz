import Model.{Answer, Player, PlayerCode, PlayerId, Quiz, QuizId}
import zio.ZIO
import zio.duration.durationInt
import zio.stm.TRef
import zio.test.Assertion._
import zio.test._
import zio.test.environment.TestEnvironment


object GameLoopTest extends DefaultRunnableSpec {

  private def createQuiz(quizState: QuizState): ZIO[Any, Throwable, Model.QuizEntity] = {
    val question1 = Model.QuestionWithSolution("q1", None, 1, Model.Choices("a", "b", "c", "d"), Answer.A)
    val question2 = Model.QuestionWithSolution("q2", None, 2, Model.Choices("a", "b", "c", "d"), Answer.A)
    quizState.createQuiz(Quiz("quiz1", List(question1, question2))).commit
  }

  private def createGame(quizState: QuizState, quizId: QuizId): ZIO[Any, Throwable, (Model.GameId, Model.AdminCode)] =
    for {
      eventQueue <- EventQueue.make[Model.Event]
      gameSetup  <- quizState.createGame("testGame", quizId, eventQueue).commit
      _ <- quizState
             .playerJoins(gameSetup.game.gameId, Player(PlayerId("1"), PlayerCode("1"), "Player1", 0, Map.empty))
             .commit
      _ <- quizState
             .playerJoins(gameSetup.game.gameId, Player(PlayerId("2"), PlayerCode("2"), "Player2", 0, Map.empty))
             .commit
    } yield gameSetup.game.gameId -> gameSetup.game.adminCode

  override def spec: ZSpec[TestEnvironment, Any] = suite("GameLoopSpec")(
    testM("wait until all players have answered") {
      for {
        state               <- QuizStateLive.quizStateLive
        quiz                <- createQuiz(state)
        (gameId, adminCode) <- createGame(state, quiz.quizId)
        _                   <- state.startGame(gameId, adminCode).commit
        done                <- TRef.make(false).commit
        _ <- (for {
               _ <- state.waitUntilAllPlayersAnswered(gameId)
               _ <- done.set(true)
             } yield ()).commit.fork
        nonAnsweredDone <- done.get.commit
        _               <- state.answer(PlayerCode("1"), gameId, Answer.A).commit
        oneAnsweredDone <- done.get.commit
        _               <- state.answer(PlayerCode("2"), gameId, Answer.B).commit
        allAnsweredDone <- done.get.commit
      } yield assert(nonAnsweredDone)(isFalse) && assert(oneAnsweredDone)(isFalse) && assert(allAnsweredDone)(isTrue)
    }
  ) @@ TestAspect.timeout(30.seconds)
}
