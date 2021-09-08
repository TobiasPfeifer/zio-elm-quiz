import Model.Quiz
import zio.test._
import zio.test.Assertion._
import zio.test.AssertionM._
import zio.test.environment.TestEnvironment
import zio.test.{Assertion, DefaultRunnableSpec, ZSpec, assertM}

object QuizSpec extends DefaultRunnableSpec {
  override def spec: ZSpec[TestEnvironment, Any] = suite("QuizSpec") (
    suite("Quizzes state") (
      testM("adding a quiz") {
        for {
          state <- QuizStateLive.quizStateLive
          quiz = Quiz("title", Nil)
          quizEntity <- state.createQuiz(quiz).commit
          quizzes <- state.quizzes.toMap.commit
        } yield assert(quizzes)(Assertion.contains(quizEntity.quizId -> quizEntity)) && assert(quizEntity.quiz)(Assertion.equalTo(quiz))
      },
      testM("querying a quiz") {
        for {
          state <- QuizStateLive.quizStateLive
          quiz = Quiz("title", Nil)
          quizEntity <- state.createQuiz(quiz).commit
          queried <- state.getQuiz(quizEntity.quizId).commit
        } yield assert(queried)(Assertion.equalTo(quizEntity)) && assert(quizEntity.quiz)(Assertion.equalTo(quiz))
      }
    )
  )
}
