import Model.{AdminCode, Answer, Choices, Game, GameId, GamePin, GameState, Player, PlayerCode, PlayerId, Question, Quiz, QuizEntity, QuizId}
import zio.Ref
import zio.duration.durationInt
import zio.stm.{STM, TMap}
import zio.test.Assertion._
import zio.test._
import zio.test.environment.TestEnvironment

object GameLoopTest extends DefaultRunnableSpec {

//  def testGameRunning(): STM[Nothing, (GameId, GameState, TMap[GameId, GameState])] = {
//    for {
//      games <- TMap.empty[GameId, GameState]
//      gameId = GameId("1")
//      quizId = QuizId("1")
//      question = Question("The answer is B", None, 1, Choices("a", "b", "c", "d")).withSolution(Answer.B)
//      game = Game(gameId, "test running", GamePin("1"), AdminCode("1"), QuizEntity(quizId, Quiz("quiz", List(question))), 1)
//      playerCode1 = PlayerCode("1")
//      playerPlayer1 = Player(PlayerId("1"), playerCode1, "player1", 0, Map())
//      playerCode2 = PlayerCode("2")
//      playerPlayer2 = Player(PlayerId("2"), playerCode2, "player2", 0, Map())
//      players = Map(playerCode1 -> playerPlayer1, playerCode2 -> playerPlayer2)
//      gameState = GameState.Running(game, players, null, 0)
//      _ <- games.put(gameId, gameState)
//    } yield (gameId, gameState, games)
//  }
//
//  def answerAllPlayers(gameId: GameId, state: TMap[GameId, GameState]): STM[String, Unit] = for {
//    gameState <- state.get(gameId).someOrFail("not found").collect({case r: GameState.Running => r})
//    updatedPlayers = gameState.players.map(p => p._2.copy(answers = p._2.answers + (gameState.questionIndex -> Answer.C)))
//    updatedGame = gameState.copy(players = updatedPlayers.map(p => p.playerCode -> p).toMap)
//    _ <- state.put(updatedGame.game.gameId, updatedGame)
//  } yield ()
//
//  def answerSinglePlayer(gameId: GameId, state: TMap[GameId, GameState]): STM[String, Unit] = for {
//    gameState <- state.get(gameId).someOrFail("not found").collect({case r: GameState.Running => r})
//    updatedPlayer = gameState.players.head._2.answer(gameState.questionIndex, Answer.B)
//    updatedPlayers = gameState.players + (updatedPlayer.playerCode -> updatedPlayer)
//    updatedState = gameState.copy(players = updatedPlayers)
//    _ <- state.put(gameId, updatedState)
//  } yield ()

  override def spec: ZSpec[TestEnvironment, Any] = suite("GameLoopSpec")(
//    testM("wait until all players have answered") {
//      for {
//        (gameId, _, state) <- testGameRunning().commit
//        done            <- Ref.make(false)
//        _ <- (for {
//               _ <- GameLoop.waitUntilAllPlayersAnswered(gameId, state)
//               _ <- done.set(true)
//             } yield ()).fork
//        nonAnsweredDone <- done.get
//        _               <- answerSinglePlayer(gameId, state).commit
//        oneAnsweredDone <- done.get.delay(10.millis).provideLayer(zio.clock.Clock.live)
//        _               <- answerAllPlayers(gameId, state).commit
//        allAnsweredDone <- done.get.delay(10.millis).provideLayer(zio.clock.Clock.live)
//      } yield (assert(nonAnsweredDone)(isFalse) && assert(oneAnsweredDone)(isFalse) && assert(allAnsweredDone)(isTrue))
//    }
  ) @@ TestAspect.timeout(30.seconds)
}
