
import Model._
import caliban.CalibanError.ValidationError
import caliban.GraphQL.graphQL
import caliban.schema.{GenericSchema, Schema}
import caliban.wrappers.ApolloTracing.apolloTracing
import caliban.wrappers.Wrappers._
import caliban.{CalibanError, GraphQL, GraphQLInterpreter, RootResolver}
import zio.console.putStrLn
import zio.duration.durationInt
import zio.stream.ZStream
import zio.{Has, IO, RIO, UIO, URIO, URLayer, ZEnv, ZIO}

import scala.language.{implicitConversions, postfixOps}

trait QuizApi {
  def interpreter(): IO[ValidationError, GraphQLInterpreter[ZEnv, CalibanError]]
}
object QuizApi {
  val live: URLayer[Has[QuizApp], Has[QuizApi]] = QuizApiLive.layer
}

case class QuizApiLive(quizApp: QuizApp) extends QuizApi {
  implicit val eventSchema = Schema.gen[Event]
  object schema extends GenericSchema[ZEnv]
  import schema._

  val api: GraphQL[ZEnv] = {
    graphQL(
      RootResolver(
        Queries(
          ZIO.succeed("Hello world!"),
          args => quizApp.getQuizzes(args.limit),
          args => quizApp.getQuiz(args.quizId)
        ),
        Mutations(
          args => quizApp.create(args),
          args => quizApp.startGame(args.adminCode, args.gameId),
          args => quizApp.answer(args.playerCode, args.gameId, args.answer),
          args => quizApp.createGame(args.title, args.quizId),
        ),
        Subscriptions(
          ZStream.repeat(()).zipWithIndex.map(_._2).map(i => s"Event $i").chunkN(1).throttleShape(1, 1.second)(_ => 1).tap(putStrLn(_)),
          args => quizApp.joinGame(args.gamePin, args.playerName)
        )
      )
    ) @@
      maxFields(1000) @@               // query analyzer that limit query fields
      maxDepth(30) @@                 // query analyzer that limit query depth
      timeout(3 seconds) @@           // wrapper that fails slow queries
      printSlowQueries(500 millis) @@ // wrapper that logs slow queries
      printErrors @@                  // wrapper that logs errors
      apolloTracing                   // wrapper for https://github.com/apollographql/apollo-tracing
  }

  override def interpreter(): IO[ValidationError, GraphQLInterpreter[ZEnv, CalibanError]] =
    api.interpreter

  case class Queries(
                      test: UIO[String],
                      quizzes: GetQuizzesArgs => RIO[ZEnv, List[QuizEntity]],
                      quiz: QuizIdArg => RIO[ZEnv, Option[QuizEntity]]
                    )

  case class Mutations(
                        createQuiz: Quiz => RIO[ZEnv, QuizEntity],
                        startGame: StartGameArgs => RIO[ZEnv, Unit],
                        vote: VoteArgs => RIO[ZEnv, Unit],
                        createGame: CreateGameArgs => RIO[ZEnv, Game],
                      )

  case class Subscriptions(
                            test: ZStream[ZEnv, Throwable, String],
                            joinGame: JoinGameArgs => ZStream[ZEnv, Throwable, Event]
                          )

  case class GetQuizzesArgs(limit: Int)
  case class QuizIdArg(quizId: QuizId)
  case class CreateGameArgs(quizId: QuizId, title: String)
  case class JoinGameArgs(gamePin: GamePin, playerName: String)
  case class VoteArgs(playerCode: PlayerCode, gameId: GameId, answer: Answer)
  case class StartGameArgs(adminCode: AdminCode, gameId: GameId)

}

object QuizApiLive {
  val layer: URLayer[Has[QuizApp], Has[QuizApi]] = {
    ZIO.service[QuizApp].map(q => QuizApiLive(q)).toLayer
  }

}
