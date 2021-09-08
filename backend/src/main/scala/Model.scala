import zio.optics.toptics._
import zio.stm.STM

import java.util.UUID

object Model {

  case class QuizId(id: String)       extends AnyVal
  case class PlayerId(id: String)     extends AnyVal
  case class PlayerCode(code: String) extends AnyVal
  case class AdminCode(code: String)  extends AnyVal
  case class GameId(id: String)       extends AnyVal
  case class GamePin(code: String)    extends AnyVal

  case class QuizEntity(quizId: QuizId, quiz: Quiz)
  case class Quiz(title: String, questions: List[QuestionWithSolution])

  case class Question(question: String, imageUrl: Option[String], points: Int, choices: Choices) { self =>
    def withSolution(answer: Answer) =
      QuestionWithSolution(self.question, self.imageUrl, self.points, self.choices, answer)
  }
  case class QuestionWithSolution(
    question: String,
    imageUrl: Option[String],
    points: Int,
    choices: Choices,
    correctAnswer: Answer
  ) {
    def withoutSolution() = Question(question, imageUrl, points, choices)
  }

  case class Choices(a: String, b: String, c: String, d: String)

  case class Game(
    gameId: GameId,
    title: String,
    pin: GamePin,
    adminCode: AdminCode,
    quizEntity: QuizEntity,
    createdTimestamp: Long
  )
  case class Player(
    playerId: PlayerId,
    playerCode: PlayerCode,
    name: String,
    score: Int,
    answers: Map[Int, Answer]
  ) { self =>
    def hasAnswered(index: Int): Boolean = self.answers.keySet.contains(index)

    def updateScore(index: Int, question: QuestionWithSolution): Player = {
      val newScore = self.answers.get(index) match {
        case Some(answer) if answer.equals(question.correctAnswer) => self.score + question.points
        case _                                                     => self.score
      }
      self.copy(score = newScore)
    }
  }

  object Player {
    def create(playerName: String): Player =
      Player(PlayerId(UUID.randomUUID().toString), PlayerCode(UUID.randomUUID().toString), playerName, 0, Map())

    object Optics {
      val answers: Lens[Player, Map[Int, Answer]] = Lens(
        p => STM.succeed(p.answers),
        a => p => STM.succeed(p.copy(answers = a))
      )
    }

  }

  sealed trait GameState {
    val game: Game
    val players: Map[PlayerCode, Player]
    val eventQueue: EventQueue[Event]
  }


  case class GameSetup(
                        override val game: Model.Game, override val players: Map[PlayerCode, Player] = Map.empty, override val eventQueue: EventQueue[Event]
                      ) extends GameState { self =>
    def addPlayer(player: Player): GameSetup = {
      self.copy(players = self.players + (player.playerCode -> player))
    }

  }

  case class RunningGame(
                          override val game: Model.Game, override val players: Map[PlayerCode, Player], override val eventQueue: EventQueue[Event],
                          questionIndex: Int
                        ) extends GameState { self =>
    def getCurrentQuestion(): Option[QuestionWithSolution] = self.game.quizEntity.quiz.questions.lift(self.questionIndex)

    def advance(): RunningGame = {
            val index          = self.questionIndex
            val question       = self.game.quizEntity.quiz.questions.lift(index)
            val updatedPlayers = question.fold(self.players)(q => self.players.view.mapValues(_.updateScore(index, q)).toMap)
            val updatedIndex   = self.questionIndex + 1

            self.copy(questionIndex = updatedIndex, players = updatedPlayers)
          }
  }

  object RunningGame {
    def start(setup: GameSetup): RunningGame = RunningGame(setup.game, setup.players, setup.eventQueue, 0)

    object Optics {
      val players: Lens[RunningGame, Map[PlayerCode, Player]] = Lens(
        g => STM.succeed(g.players),
        p => g => STM.succeed(g.copy(players = p))
      )

      val currentQuestionIndex: Lens[RunningGame, Int] = Lens(
        g => STM.succeed(g.questionIndex),
        i => g => STM.succeed(g.copy(questionIndex = i))
      )

      def playerAnswers(playerCode: PlayerCode): Optional[RunningGame, Map[Int, Answer]] =
        Optic.identity[RunningGame] >>> RunningGame.Optics.players >>> Optic.key(playerCode) >>> Player.Optics.answers

      def currentPlayerAnswer(playerCode: PlayerCode): Optional[RunningGame, Answer] =
        playerAnswers(playerCode).zip(Optic.identity[RunningGame] >>> currentQuestionIndex) >>> Optional(
          t => STM.fromEither(t._1.get(t._2).toRight(OpticFailure(s"player with code $playerCode has not answered question at index ${t._2} yet"))),
          (a: Answer) => t => STM.succeed(t._1 + (t._2 -> a), t._2)
        )

      val currentQuestion: Optional[RunningGame, QuestionWithSolution] = Optional(
        (g: RunningGame) => STM.fromEither(g.getCurrentQuestion().toRight(OpticFailure(s"quiz ${g.game.quizEntity.quizId} in game ${g.game.gameId} has no question at index ${g.questionIndex}"))),
        (_: QuestionWithSolution) => (_: RunningGame) => STM.fail(OpticFailure("operation not supported"))
      )

    }
  }

  sealed trait Answer
  object Answer {
    case object A extends Answer
    case object B extends Answer
    case object C extends Answer
    case object D extends Answer
  }


  sealed trait Event

  object Event {
    case class GameStarted(gameId: GameId)                                                        extends Event
    case class PlayerJoined(playerId: PlayerId, name: String, allPlayers: Map[PlayerId, String])  extends Event
    case class PlayerCodeCreated(playerId: PlayerId, playerCode: PlayerCode, gameId: GameId)      extends Event
    case class NextQuestion(question: Question)                                                   extends Event
    case class QuestionEnded(question: QuestionWithSolution, leaderBoard: List[LeaderboardEntry]) extends Event
    case class GameFinished(game: Game, leaderBoard: List[LeaderboardEntry])                      extends Event

    case class LeaderboardEntry(playerId: PlayerId, playerName: String, score: Int)
  }
}
