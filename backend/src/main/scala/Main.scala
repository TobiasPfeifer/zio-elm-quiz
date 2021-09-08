import caliban.ZHttpAdapter
import zhttp.http.Http
import zhttp.service.Server
import zio._
import zio.stream._
import zhttp.http._
import zio.console.{getStrLn, putStrLn}
import zio.magic.ZioProvideMagicOps

object Main extends App {

  val port = 8080

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
    program
      .inject(ZEnv.live, QuizApp.live, QuizApi.live, QuizState.live)
      .exitCode

  lazy val program: ZIO[zio.ZEnv with Has[QuizApi], Throwable, Unit] = {
    val exit = "quit"

    def stop(s: Fiber.Runtime[Throwable, Unit]) = for {
      _            <- putStrLn(s"Type '${exit}' to stop the server")
      input        <- getStrLn
      isExitCommand = exit.equalsIgnoreCase(input)
      _            <- (putStrLn("shutting down ") *> s.interrupt).when(isExitCommand)
    } yield isExitCommand

    for {
      _ <- putStrLn(s"starting server on port ${port}")
      s <- buildServer().forkDaemon
      _ <- putStrLn(s"server started. Press")
      _ <- stop(s).repeatWhile(_ == false)
    } yield ()
  }

  def buildServer(): ZIO[zio.ZEnv with Has[QuizApi], Throwable, Unit] = {
    for {
      service <- ZIO.service[QuizApi]
      interpreter <- service.interpreter()
      _           <- Server
        .start(
          port,
          CORS(
            Http.route {
              case _ -> Root / "api" / "graphql" => ZHttpAdapter.makeHttpService(interpreter)
              case _ -> Root / "ws" / "graphql" => ZHttpAdapter.makeWebSocketService(interpreter)
              case _ -> Root / "graphiql" => Http.succeed(Response.http(content = HttpData.fromStream(ZStream.fromResource("graphiql.html"))))
            },
            config = CORSConfig(anyOrigin = true)
          )
        )
        .forever
    } yield ()
  }

}
