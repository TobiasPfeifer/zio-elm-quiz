port module Main exposing (main)

import Browser
import Debug exposing (toString)
import Graphql.Document
import Graphql.Http exposing (Error)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument as OptionalArgument
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Html exposing (Html, a, b, button, div, h1, h2, h3, hr, input, li, text, ul)
import Html.Attributes exposing (checked, class, href, id, name, placeholder, style, target, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
import Json.Encode
import List
import Quiz.Enum.Answer as Answer exposing (Answer)
import Quiz.InputObject exposing (QuestionWithSolutionInput)
import Quiz.Mutation as Mutation exposing (CreateGameRequiredArguments, CreateQuizRequiredArguments)
import Quiz.Object
import Quiz.Object.Choices
import Quiz.Object.Game
import Quiz.Object.GameFinished
import Quiz.Object.GameStarted
import Quiz.Object.KVStringString
import Quiz.Object.LeaderboardEntry
import Quiz.Object.NextQuestion
import Quiz.Object.PlayerCodeCreated
import Quiz.Object.PlayerJoined
import Quiz.Object.Question
import Quiz.Object.QuestionEnded
import Quiz.Object.QuestionWithSolution
import Quiz.Object.Quiz
import Quiz.Object.QuizEntity
import Quiz.Query as Query
import Quiz.Scalar exposing (Unit)
import Quiz.Subscription as Subscription exposing (JoinGameRequiredArguments)
import Quiz.Union
import Quiz.Union.Event
import RemoteData exposing (RemoteData(..))
import String
import Quiz.Object.Game
import Tuple exposing (pair)

type alias CreatedGame = { title: String, pin: String, gameId: GameId, adminCode: String}

type alias JoinedGameModel =
    { me: Maybe PlayerIdentity
    , gameId: Maybe GameId
    , players : List (PlayerId, PlayerName)
    }

type alias CreateGameModel =
    { title: String
    , selectedQuiz: Maybe QuizId
    }

type alias RunningGameModel =
    { me: PlayerIdentity
    , gameId: GameId
    , questionState: QuestionState
    }

type QuestionState
    = Ask Question
    | Answered Question Answer
    | Solution QuestionWithSolution LeaderBoard
    | WaitingForQuestion

type alias Choices = Quiz.InputObject.ChoicesInput


emptyChoices : Choices
emptyChoices =
    { a = "", b = "", c = "", d = "" }


type alias Question =
    { question : String
    , imgUrl : Maybe String
    , choices : Choices
    , points : Int
    }

type alias QuestionWithSolution = QuestionWithSolutionInput


emptyQuestionWithSolution : QuestionWithSolution
emptyQuestionWithSolution =
    { question = "", imageUrl = OptionalArgument.Absent, choices = emptyChoices, correctAnswer = Answer.A, points = 0 }


type alias QuizWithSolution = CreateQuizRequiredArguments

type alias Quiz =
    { title : String
    , questions : List Question
    }

type alias QuizId = String

type alias QuizListItem =
    { title : String
    , id: QuizId
    }

type alias QuizCreatedResponse = { id: QuizId }

type alias QuizEntity =
    { id: QuizId,
      quiz: Quiz
    }

type alias Title = String

type alias GamePin = String


emptyJoinGameArgs : JoinGameRequiredArguments
emptyJoinGameArgs = { gamePin = "", playerName = ""}

type Model
    = Initial JoinGameRequiredArguments
    | CreateQuiz QuizWithSolution (GraphQLResponse (Maybe QuizCreatedResponse))
    | CreateGame Title (GraphQLResponse Quizzes)
    | CreateGameQuizSelected Title QuizId
    | GameCreated (GraphQLResponse (Maybe CreatedGame))
    | JoinedGame JoinedGameModel
    | GameRunning RunningGameModel
    | GameEnd LeaderBoard


type Msg
    = SwitchModelCreateQuiz
    | SwitchModeCreateGame
    | CreateQuizSetTitle String
    | CreateQuizAddQuestion
    | CreateQuizRemoveQuestion Int
    | CreateQuizUpdateQuestion Int String
    | CreateQuizUpdateAnswer Int (Choices -> String -> Choices) String
    | CreateQuizCorrectAnswer Int Answer
    | CreateQuizCancel
    | CreateQuizSubmit
    | CreateQuizCreated (GraphQLResponse (Maybe QuizCreatedResponse))
    | CreateGameQuizzesLoaded (GraphQLResponse Quizzes)
    | CreateGameForQuizId QuizId
    | CreateGameSetTitle String
    | GameCreatedResult (GraphQLResponse (Maybe CreatedGame))
    | CreateGameCancel
    | CreateGameSubmit
    | CreateGameSelectQuiz QuizId
    | InitialSetGamePin String
    | InitialSetPlayerName String
    | InitialJoinGame
    | StartGame GameId String
    | SubmitAnswer Answer
    | QuestionAnswered
    | ReceivedGameEvent Decode.Value

type alias Quizzes =
    List QuizListItem

type alias MutationResponse =
    { affected_rows : Int
    }

type alias GameId = String
type alias PlayerId = String
type alias PlayerName = String
type alias PlayerCode = String

type alias MaybeMutationResponse =
    Maybe MutationResponse

type alias GraphQLResponse decodesTo
    = RemoteData (Graphql.Http.Error decodesTo) decodesTo


--port consoleLog : String -> Cmd msg



--port createTestSubscription : String -> Cmd msg
--port receivedTestSubscriptionData : (String -> msg) -> Sub msg
port joinGame: String -> Cmd msg
port receivedGameEvent : (Decode.Value -> msg) -> Sub msg

graphql_url = "http://localhost:8080/api/graphql"

makeGraphQLQuery : SelectionSet decodesTo RootQuery -> (Result (Graphql.Http.Error decodesTo) decodesTo -> msg) -> Cmd msg
makeGraphQLQuery query decodesTo =
    query
        |> Graphql.Http.queryRequest graphql_url
        |> Graphql.Http.send decodesTo

makeGraphQLMutation : SelectionSet decodesTo RootMutation -> (Result (Graphql.Http.Error decodesTo) decodesTo -> msg) -> Cmd msg
makeGraphQLMutation mutation decodesTo =
    mutation
        |> Graphql.Http.mutationRequest graphql_url
        |> Graphql.Http.send decodesTo

quizListItemSelection : SelectionSet QuizListItem Quiz.Object.QuizEntity
quizListItemSelection =
    SelectionSet.map2 QuizListItem (Quiz.Object.QuizEntity.quiz Quiz.Object.Quiz.title) Quiz.Object.QuizEntity.quizId

query_loadQuizzes : Cmd Msg
query_loadQuizzes =
    makeGraphQLQuery (Query.quizzes ({ limit = 20 }) quizListItemSelection )
        (RemoteData.fromResult >> CreateGameQuizzesLoaded)

--questionsSelection : SelectionSet Question Quiz.Object.QuestionWithSolution
--questionsSelection = SelectionSet.empty
--
--quizSelection : SelectionSet Quiz Quiz.Object.Quiz
--quizSelection = SelectionSet.map2 Quiz Quiz.Object.Quiz.title (Quiz.Object.Quiz.questions questionsSelection)


quizEntitySelection : SelectionSet QuizCreatedResponse Quiz.Object.QuizEntity
quizEntitySelection =
    SelectionSet.map QuizCreatedResponse Quiz.Object.QuizEntity.quizId

mutation_saveQuiz : CreateQuizRequiredArguments -> Cmd Msg
mutation_saveQuiz quiz =
    makeGraphQLMutation (Mutation.createQuiz { quiz |  questions = List.map(\i -> {i | points = 1}) quiz.questions} (quizEntitySelection)) (RemoteData.fromResult >> CreateQuizCreated)

mutation_startGame : GameId -> String -> Cmd Msg
mutation_startGame gameId adminCode =
    makeGraphQLMutation (Mutation.startGame {adminCode = adminCode, gameId = gameId}) (\_ -> SwitchModeCreateGame)

mutaion_answerQuestion : Answer -> PlayerCode -> GameId -> Cmd Msg
mutaion_answerQuestion answer playerCode gameId =
    makeGraphQLMutation (Mutation.vote { playerCode = playerCode, gameId = gameId, answer = answer}) (\_ -> QuestionAnswered)

createGameSelection : SelectionSet CreatedGame Quiz.Object.Game
createGameSelection =
    SelectionSet.succeed CreatedGame
            |> with (Quiz.Object.Game.title)
            |> with (Quiz.Object.Game.pin)
            |> with (Quiz.Object.Game.gameId)
            |> with (Quiz.Object.Game.adminCode)

mutation_createGame : CreateGameRequiredArguments -> Cmd Msg
mutation_createGame args =
    makeGraphQLMutation (Mutation.createGame args (createGameSelection)) (RemoteData.fromResult >> GameCreatedResult)

init : () -> ( Model, Cmd Msg )
init flags =
    ( Initial emptyJoinGameArgs, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Initial joinArgs ->
            updateInitial msg model joinArgs

        CreateQuiz quiz _ ->
            updateCreateQuiz msg quiz

        CreateGame title quizzesResponse ->
            updateCreateGame msg title quizzesResponse

        CreateGameQuizSelected title quizId ->
            updateCreateGameQuizSelected msg title quizId

        GameCreated result ->
            updateGameCreated msg result

        JoinedGame joinedGameModel ->
            updateJoinedGame msg joinedGameModel

        GameRunning runningGameModel ->
            updateRunningGame msg runningGameModel

        _ ->
            ( model, Cmd.none )

decodeEventData : Decode.Value -> RemoteData Decode.Error GameEvent
decodeEventData data = Decode.decodeValue (joinGameSubscriptionSelection |> Graphql.Document.decoder) data |> RemoteData.fromResult

log : String -> a -> b -> b
log msg obj ret =
    case (Debug.log msg obj) of
        _ -> ret


onReceivedGameEvent : a -> Decode.Value -> (a -> GameEvent -> Maybe (Model, Cmd Msg)) -> (a -> (Model, Cmd Msg)) -> (Model, Cmd Msg)
onReceivedGameEvent model data fUpdate fIgnore=
    case (decodeEventData (log "ReceivedGameEvent data" (Json.Encode.encode 2 data) data)) of
        Success gameEvent ->
            case fUpdate model (log "Parsed GameEvent" gameEvent gameEvent) of
                Just a -> log "handled GameEvent" { gameEvent = gameEvent, model = model } a
                Nothing -> log "unhandled GameEvent" { gameEvent = gameEvent, model = model } (fIgnore model)

        Failure e -> log "decoding error" {data = (Json.Encode.encode 2 data), e = e} (fIgnore model)
        _ -> log "decoding error" (Json.Encode.encode 2 data) (fIgnore model)



updateJoinedGame : Msg -> JoinedGameModel -> (Model, Cmd Msg)
updateJoinedGame msg model =
    let
        fUpdate : JoinedGameModel -> GameEvent -> Maybe (Model, Cmd Msg)
        fUpdate m e =
            case e of
                PlayerCodeCreated record ->
                    case m.gameId of
                        Just gameId -> Just ( GameRunning { me = record, gameId = gameId, questionState = WaitingForQuestion }, Cmd.none)
                        Nothing -> Just ( JoinedGame { m | me = Just record }, Cmd.none)

                PlayerJoined record -> Just (JoinedGame { m | players = record }, Cmd.none)

                GameStarted gameId ->
                    case m.me of
                        Just me -> Just ( GameRunning { me = me, gameId = gameId, questionState = WaitingForQuestion }, Cmd.none)
                        Nothing -> Just ( JoinedGame { m | gameId = Just gameId }, Cmd.none )

                _ -> Nothing

        fFailure : JoinedGameModel -> (Model, Cmd Msg)
        fFailure m = (JoinedGame m , Cmd.none)
    in
        case msg of
            ReceivedGameEvent data -> onReceivedGameEvent model data fUpdate fFailure
            _ -> fFailure model

updateRunningGame : Msg -> RunningGameModel -> (Model, Cmd Msg)
updateRunningGame msg model =
    let
        fUpdate : RunningGameModel -> GameEvent -> Maybe (Model, Cmd Msg)
        fUpdate m e =
            case e of
                GameFinished leaderBoard -> Just ( GameEnd leaderBoard, Cmd.none )

                NextQuestion question -> Just ( GameRunning { m | questionState = Ask question }, Cmd.none)

                QuestionEnded questionWithSolution leaderBoard -> Just ( GameRunning { m | questionState = Solution questionWithSolution leaderBoard }, Cmd.none)

                _ -> Nothing

        fFailure : RunningGameModel -> (Model, Cmd Msg)
        fFailure m = (GameRunning m , Cmd.none)
    in
        case msg of
            ReceivedGameEvent data -> onReceivedGameEvent model data fUpdate fFailure
            SubmitAnswer answer ->
                case model.questionState of
                    Ask question -> ((GameRunning {model | questionState = Answered question answer}, mutaion_answerQuestion answer model.me.playerCode model.gameId))
                    _ -> fFailure model
            QuestionAnswered ->
                (GameRunning model, Cmd.none)

            _ -> fFailure model



updateInitial : Msg -> Model -> JoinGameRequiredArguments -> ( Model, Cmd Msg )
updateInitial msg model joinArgs =
    case msg of
        SwitchModelCreateQuiz ->
            ( CreateQuiz { title = "New Quiz Title", questions = [] } RemoteData.NotAsked, Cmd.none )

        SwitchModeCreateGame ->
            ( CreateGame "New Game Title" RemoteData.Loading, query_loadQuizzes )

        InitialSetGamePin pin ->
            ( Initial { joinArgs | gamePin = pin }, Cmd.none)

        InitialSetPlayerName name ->
            ( Initial { joinArgs | playerName = name }, Cmd.none)

        InitialJoinGame ->
            ( JoinedGame { players = [], me = Nothing, gameId = Nothing } , joinGame (joinGameSubscription joinArgs |> Graphql.Document.serializeSubscription))

        _ ->
            ( model, Cmd.none )

type alias PlayerIdentity = { playerId: PlayerId, playerCode: PlayerCode }

type GameEvent
    = GameFinished LeaderBoard
    | GameStarted GameId
    | NextQuestion Question
    | PlayerCodeCreated PlayerIdentity
    | PlayerJoined (List (PlayerId, PlayerName))
    | QuestionEnded QuestionWithSolution LeaderBoard

type alias LeaderBoard = List LeaderBoardEntry
type alias LeaderBoardEntry = { playerId: PlayerId, playerName: PlayerName, score: Int}


joinGameSubscription : JoinGameRequiredArguments -> SelectionSet (Maybe GameEvent) RootSubscription
joinGameSubscription args = Subscription.joinGame args joinGameSubscriptionSelection

playerJoinedSelection : SelectionSet GameEvent Quiz.Object.PlayerJoined
playerJoinedSelection =
    let
        innerSelection : SelectionSet (PlayerId, PlayerName) Quiz.Object.KVStringString
        innerSelection = SelectionSet.succeed pair -- try lambda first?
            |> with Quiz.Object.KVStringString.key
            |> with Quiz.Object.KVStringString.value
    in
        SelectionSet.succeed PlayerJoined
            |> with (Quiz.Object.PlayerJoined.allPlayers innerSelection)

playerCodeCreatedSelection : SelectionSet GameEvent Quiz.Object.PlayerCodeCreated
playerCodeCreatedSelection =
    let
        playerCodeCreatedConstructor : PlayerId -> PlayerCode -> GameEvent
        playerCodeCreatedConstructor id code = PlayerCodeCreated { playerId = id, playerCode = code }
    in
        SelectionSet.succeed playerCodeCreatedConstructor
            |> with (Quiz.Object.PlayerCodeCreated.playerId)
            |> with (Quiz.Object.PlayerCodeCreated.playerCode)

gameStartedSelection : SelectionSet GameEvent Quiz.Object.GameStarted
gameStartedSelection = SelectionSet.map GameStarted Quiz.Object.GameStarted.gameId

choicesSelection : SelectionSet Choices Quiz.Object.Choices
choicesSelection = SelectionSet.succeed (\a -> \b -> \c -> \d -> {a = a, b = b, c = c, d = d})
    |> with Quiz.Object.Choices.a
    |> with Quiz.Object.Choices.b
    |> with Quiz.Object.Choices.c
    |> with Quiz.Object.Choices.d

nextQuestionSelection : SelectionSet GameEvent Quiz.Object.NextQuestion
nextQuestionSelection =
    let
        questionConstructor : String -> Maybe String -> Choices -> Int -> Question
        questionConstructor question imgUrl choices points = { question = question, imgUrl = imgUrl, choices = choices, points = points }

        innerSelection : SelectionSet Question Quiz.Object.Question
        innerSelection = SelectionSet.succeed questionConstructor
            |> with Quiz.Object.Question.question
            |> with Quiz.Object.Question.imageUrl
            |> with (Quiz.Object.Question.choices choicesSelection)
            |> with Quiz.Object.Question.points

    in
        SelectionSet.succeed NextQuestion
            |> with (Quiz.Object.NextQuestion.question innerSelection)

leaderboardEntrySelection : SelectionSet LeaderBoardEntry Quiz.Object.LeaderboardEntry
leaderboardEntrySelection = SelectionSet.succeed LeaderBoardEntry
    |> with Quiz.Object.LeaderboardEntry.playerId
    |> with Quiz.Object.LeaderboardEntry.playerName
    |> with Quiz.Object.LeaderboardEntry.score

questionEndedSelection : SelectionSet GameEvent Quiz.Object.QuestionEnded
questionEndedSelection =
    let
        questionConstructor : String -> Maybe String -> Choices -> Int -> Answer -> QuestionWithSolution
        questionConstructor question imgUrl choices points solution = { question = question, imageUrl = OptionalArgument.fromMaybe imgUrl, choices = choices, points = points, correctAnswer = solution }

        questionSelection : SelectionSet QuestionWithSolution Quiz.Object.QuestionWithSolution
        questionSelection = SelectionSet.succeed questionConstructor
            |> with Quiz.Object.QuestionWithSolution.question
            |> with Quiz.Object.QuestionWithSolution.imageUrl
            |> with (Quiz.Object.QuestionWithSolution.choices choicesSelection)
            |> with Quiz.Object.QuestionWithSolution.points
            |> with Quiz.Object.QuestionWithSolution.correctAnswer

    in
        SelectionSet.succeed QuestionEnded
            |> with (Quiz.Object.QuestionEnded.question questionSelection)
            |> with (Quiz.Object.QuestionEnded.leaderBoard leaderboardEntrySelection)


gameFinishedSelection : SelectionSet GameEvent Quiz.Object.GameFinished
gameFinishedSelection = SelectionSet.succeed GameFinished
    |> with (Quiz.Object.GameFinished.leaderBoard leaderboardEntrySelection)


joinGameSubscriptionFragment : Quiz.Union.Event.Fragments GameEvent
joinGameSubscriptionFragment =
    { onGameFinished = gameFinishedSelection
    , onGameStarted = gameStartedSelection
    , onNextQuestion = nextQuestionSelection
    , onPlayerCodeCreated = playerCodeCreatedSelection
    , onPlayerJoined = playerJoinedSelection
    , onQuestionEnded = questionEndedSelection
    }

joinGameSubscriptionSelection : SelectionSet GameEvent Quiz.Union.Event
joinGameSubscriptionSelection = Quiz.Union.Event.fragments joinGameSubscriptionFragment


updateCreateQuiz : Msg -> QuizWithSolution -> ( Model, Cmd Msg )
updateCreateQuiz msg quiz =
    case msg of
        CreateQuizSetTitle title ->
            ( CreateQuiz { quiz | title = title } RemoteData.NotAsked, Cmd.none )

        CreateQuizAddQuestion ->
            ( CreateQuiz { quiz | questions = quiz.questions ++ [ emptyQuestionWithSolution ] } RemoteData.NotAsked, Cmd.none )

        CreateQuizRemoveQuestion index ->
            ( CreateQuiz { quiz | questions = removeAtIndex index quiz.questions } RemoteData.NotAsked, Cmd.none )

        CreateQuizUpdateQuestion index input ->
            ( CreateQuiz { quiz | questions = updateAtIndex index quiz.questions (\q -> { q | question = input }) } RemoteData.NotAsked, Cmd.none )

        CreateQuizUpdateAnswer index updateChoice input ->
            ( CreateQuiz { quiz | questions = updateAtIndex index quiz.questions (\q -> { q | choices = updateChoice q.choices input }) } RemoteData.NotAsked, Cmd.none )

        CreateQuizCorrectAnswer index answer ->
            ( CreateQuiz { quiz | questions = updateAtIndex index quiz.questions (\q -> { q | correctAnswer = answer }) } RemoteData.NotAsked, Cmd.none )

        CreateQuizSubmit ->
            ( CreateQuiz quiz  RemoteData.Loading, mutation_saveQuiz quiz )

        CreateQuizCancel ->
            ( Initial emptyJoinGameArgs, Cmd.none )

        CreateQuizCreated response ->
            (CreateQuiz quiz response, Cmd.none)

        CreateGameForQuizId quizId ->
            (CreateGameQuizSelected "New Game" quizId, Cmd.none)

        _ ->
            ( Initial emptyJoinGameArgs, Cmd.none )

updateCreateGame : Msg -> Title -> (GraphQLResponse Quizzes) -> ( Model, Cmd Msg )
updateCreateGame msg title quizzesResponse =
    case msg of
        CreateGameQuizzesLoaded response ->
            (CreateGame title response, Cmd.none)

        CreateGameSelectQuiz quizId ->
            ( CreateGameQuizSelected title quizId , Cmd.none )

        _ ->
             (CreateGame title quizzesResponse, Cmd.none)

updateGameCreated : Msg -> (GraphQLResponse (Maybe CreatedGame)) -> (Model, Cmd Msg)
updateGameCreated msg result =
    case msg of
        GameCreatedResult response ->
            (GameCreated response, Cmd.none)

        StartGame gameId adminCode ->
                (Initial emptyJoinGameArgs, mutation_startGame gameId adminCode)

        _ ->
            (Initial emptyJoinGameArgs, Cmd.none)
        

updateCreateGameQuizSelected : Msg -> Title -> QuizId -> (Model, Cmd Msg)
updateCreateGameQuizSelected msg title quizId =
    case msg of
        CreateGameSetTitle newTitle ->
            (CreateGameQuizSelected newTitle quizId, Cmd.none)

        CreateGameSubmit ->
            (GameCreated RemoteData.Loading, mutation_createGame {title = title, quizId = quizId })

        CreateGameCancel ->
            (Initial emptyJoinGameArgs, Cmd.none)

        _ -> (Initial emptyJoinGameArgs, Cmd.none)



removeAtIndex : Int -> List a -> List a
removeAtIndex i xs =
    List.take i xs ++ List.drop (i + 1) xs


updateAtIndex : Int -> List a -> (a -> a) -> List a
updateAtIndex i xs f =
    let
        mapping idx question =
            if idx == i then
                f question

            else
                question
    in
    List.indexedMap mapping xs



subscriptions : Model -> Sub Msg
subscriptions model =
    receivedGameEvent ReceivedGameEvent


view : Model -> Html.Html Msg
view model =
    div [class "container"]
        [ div []
            [ h1 [] [ text "ZIO-Elm-Quiz" ]
            , h2 [] [ a [ href "https://github.com/TobiasPfeifer", target "_blank" ] [ text "Sources available on GitHub" ] ]
            ]
        , div []
            [ renderContent model
            ]
        ]



renderContent : Model -> Html.Html Msg
renderContent model =
    case model of
        Initial joinGameArgs ->
            renderInitial model joinGameArgs

        CreateQuiz quiz response ->
            renderCreateQuiz quiz response

        CreateGame title response ->
            renderCreateGame title response

        CreateGameQuizSelected title quizId ->
            renderCreateGameQuizSelected title quizId

        GameCreated response ->
            renderGameCreated response

        JoinedGame joinedGameModel ->
            renderJoinedGame joinedGameModel

        GameRunning gameRunningModel ->
            renderGameRunning gameRunningModel

        GameEnd leaderboard ->
            renderLeaderboard leaderboard


renderInitial : Model -> JoinGameRequiredArguments -> Html.Html Msg
renderInitial _ joinGameArgs =
    div []
        [ button [ onClick SwitchModelCreateQuiz ] [ text "Create a Quiz" ]
        , button [ onClick SwitchModeCreateGame ] [ text "Start a Game"]
        , div []
            [ input [ type_ "text", name "pin", placeholder "Game Pin", value joinGameArgs.gamePin, onInput InitialSetGamePin ] []
            , input [ type_ "text", name "name", placeholder "Player Name", value joinGameArgs.playerName, onInput InitialSetPlayerName ] []
            , button [ onClick InitialJoinGame ] [ text "Join Game"]
            ]

        ]

renderJoinedGame : JoinedGameModel -> Html.Html Msg
renderJoinedGame joinedGameModel =
    let
       gameTitle = Maybe.map (\s -> String.concat ["Game ", s])  joinedGameModel.gameId |> Maybe.withDefault "Game"

       renderParticipant : (PlayerId, PlayerName) -> Html msg
       renderParticipant (_, name) = li [] [text name]
    in
        div []
            [ h2 [] [text gameTitle]
            , h3 [] [text "Players"]
            , ul [] (List.map renderParticipant joinedGameModel.players)
            ]

renderLeaderboard : LeaderBoard -> Html.Html Msg
renderLeaderboard leaderboard =
    div []
    [ h2 [] [text "Leaderboard"]
    , ul [] (List.map renderLeaderboardEntry leaderboard)
    ]

renderLeaderboardEntry : LeaderBoardEntry -> Html.Html Msg
renderLeaderboardEntry entry =
    li [] [ String.concat [entry.playerName, "\t-\t", String.fromInt entry.score] |> text]

renderGameRunning : RunningGameModel -> Html.Html Msg
renderGameRunning runningGameModel =
    let
        wrapSelectedInBold elementAnser givenAnswer text =
            if elementAnser == givenAnswer then b [] [text] else text
    in
        case runningGameModel.questionState of
            Ask question ->
                div []
                [ h2 [] [text question.question]
                , ul []
                    [ li [] [a [onClick (SubmitAnswer Answer.A)] [String.concat ["A: ", question.choices.a] |> text]]
                    , li [] [a [onClick (SubmitAnswer Answer.B)] [String.concat ["B: ", question.choices.b] |> text]]
                    , li [] [a [onClick (SubmitAnswer Answer.C)] [String.concat ["C: ", question.choices.c] |> text]]
                    , li [] [a [onClick (SubmitAnswer Answer.D)] [String.concat ["D: ", question.choices.d] |> text]]
                    ]
                ]

            Answered question answer ->
                div []
                [ h2 [] [text question.question]
                , ul []
                    [ li [] [String.concat ["A: ", question.choices.a] |> text |> wrapSelectedInBold Answer.A answer]
                    , li [] [String.concat ["B: ", question.choices.b] |> text |> wrapSelectedInBold Answer.B answer]
                    , li [] [String.concat ["C: ", question.choices.c] |> text |> wrapSelectedInBold Answer.C answer]
                    , li [] [String.concat ["D: ", question.choices.d] |> text |> wrapSelectedInBold Answer.D answer]
                    ]
                ]
            Solution questionWithSolution leaderBoard ->
                div []
                [ h2 [] [text questionWithSolution.question]
                , ul []
                    [ li [] [String.concat ["A: ", questionWithSolution.choices.a] |> text]
                    , li [] [String.concat ["B: ", questionWithSolution.choices.b] |> text]
                    , li [] [String.concat ["C: ", questionWithSolution.choices.c] |> text]
                    , li [] [String.concat ["D: ", questionWithSolution.choices.d] |> text]
                    ]
                , h2 [] [String.concat ["Correct Answer: ", answerToString questionWithSolution.correctAnswer] |> text]
                , renderLeaderboard leaderBoard
                ]



            WaitingForQuestion -> loading


errorDisplay : String -> Html.Html Msg
errorDisplay message = div [] [h2 [] [text message]]

errorToString : Error a -> String
errorToString error = "error in graphql request"

loading : Html msg
loading = div [] [ text "Loading ..."]

renderCreateQuiz : QuizWithSolution -> (GraphQLResponse (Maybe QuizCreatedResponse)) -> Html.Html Msg
renderCreateQuiz quiz response =
    case response of
        NotAsked ->
                div []
                    [ input [ type_ "text", name "title", placeholder "Title", value quiz.title, onInput CreateQuizSetTitle ] []
                    , div [] (List.indexedMap renderCreateQuestion quiz.questions ++ [ button [ style "margin-top" "2em", onClick CreateQuizAddQuestion ] [ text "Add Question" ] ])
                    , div [ style "margin-top" "1em" ] [ button [ onClick CreateQuizCancel ] [ text "Cancel" ], button [ onClick CreateQuizSubmit ] [ text "Submit" ] ]
                    ]

        Loading -> loading


        Failure e -> errorToString e |> errorDisplay


        Success a ->
            case a of
                Just quizCreatedResponse ->
                    div []
                        [ h3 [] [["Successfully created quiz '", quiz.title, "' with id ", quizCreatedResponse.id] |> String.concat |> text]
                        , div [ style "margin-top" "1em" ] [ button [ onClick (CreateGameForQuizId quizCreatedResponse.id) ] [ text "Create Game" ]]
                       ]


                Nothing -> errorDisplay "quizCreatedResponse not present"


answerToString : Answer -> String
answerToString a =
    case a of
        Answer.A -> "A"
        Answer.B -> "B"
        Answer.C -> "C"
        Answer.D -> "D"

renderCreateQuestion : Int -> QuestionWithSolution -> Html.Html Msg
renderCreateQuestion index question =
    let
        idString : String -> Answer -> String
        idString t s =
            String.concat [ "question", String.fromInt index, "_", t, answerToString s ]

        answerString : Answer -> String
        answerString answerType =
            String.concat [ "Answer ", answerToString answerType ]

        answerDiv : Answer -> String -> Answer -> (Choices -> String -> Choices) ->  Html Msg
        answerDiv answerType field correctAnswer choiceUpdate =
            div []
                [ input [ type_ "text", id <| idString "option" answerType, placeholder <| (answerString answerType), value field, onInput <| CreateQuizUpdateAnswer index choiceUpdate ] []
                , input [ type_ "radio", name "correctAnswer", checked (answerType == correctAnswer), id <| idString "option" answerType, value <| answerToString answerType, onInput (\_ -> CreateQuizCorrectAnswer index answerType) ] []
                ]
    in
        div []
            [ hr [] []
            , h2 []
                [ text (String.concat [ "Question ", String.fromInt (index + 1) ])
                , button [ style "margin-left" "6em", onClick (CreateQuizRemoveQuestion index) ] [ text "remove question" ]
                ]
            , input [ type_ "text", name "question", placeholder "Question", value question.question, onInput (CreateQuizUpdateQuestion index) ] []
            , div [ style "margin-top" "1em" ]
                [ answerDiv Answer.A question.choices.a question.correctAnswer (\choices s -> { choices | a = s })
                , answerDiv Answer.B question.choices.b question.correctAnswer (\choices s -> { choices | b = s })
                , answerDiv Answer.C question.choices.c question.correctAnswer (\choices s -> { choices | c = s })
                , answerDiv Answer.D question.choices.d question.correctAnswer (\choices s -> { choices | d = s })

                --, div []
                --    [ input [ type_ "text", id <| idString "optionA", placeholder "Answer B", value question.choices.b, onInput (CreateQuizUpdateAnswer index (\choices s -> { choices | b = s })) ] []
                --    , input [ type_ "radio", name "correctAnswer", id "correctB", value "B", onInput (\_ -> CreateQuizCorrectAnswer index Answer.B) ] []
                --    ]
                --, div []
                --    [ input [ type_ "text", id <| idString "optionA", placeholder "Answer C", value question.choices.c, onInput (CreateQuizUpdateAnswer index (\choices s -> { choices | c = s })) ] []
                --    , input [ type_ "radio", name "correctAnswer", id "correctC", value "C", onInput (\_ -> CreateQuizCorrectAnswer index Answer.C) ] []
                --    ]
                --, div []
                --    [ input [ type_ "text", id <| idString "optionA", placeholder "Answer D", value question.choices.d, onInput (CreateQuizUpdateAnswer index (\choices s -> { choices | d = s })) ] []
                --    , input [ type_ "radio", name "correctAnswer", id "correctD", value "D", onInput (\_ -> CreateQuizCorrectAnswer index Answer.D) ] []
                --    ]
                ]
            ]

createGameTitleInput title = input [ type_ "text", name "title", placeholder "Title", value title, onInput CreateGameSetTitle ] []

renderCreateGame : Title -> (GraphQLResponse Quizzes) -> Html.Html Msg
renderCreateGame title response =
    case response of
        NotAsked ->
            div [] [ button [ onClick SwitchModeCreateGame ] [ text "load Quizzes"]]

        Loading -> loading


        Failure e -> errorToString e |> errorDisplay


        Success quizzes ->
            let
                renderQuiz : QuizListItem -> Html.Html Msg
                renderQuiz quiz =
                    div []
                    [ text quiz.title, button [ onClick (CreateGameSelectQuiz quiz.id) ] [ text "Select"]
                    ]
            in
                div [] (List.map renderQuiz quizzes)


renderCreateGameQuizSelected : Title -> QuizId -> Html.Html Msg
renderCreateGameQuizSelected title _ =
       div []
           [ input [ type_ "text", name "title", placeholder "Title", value title, onInput CreateGameSetTitle ] []
           , div [ style "margin-top" "1em" ] [ button [ onClick CreateGameCancel ] [ text "Cancel" ], button [ onClick CreateGameSubmit ] [ text "Submit" ] ]
           ]

renderGameCreated : (GraphQLResponse (Maybe CreatedGame)) -> Html.Html Msg
renderGameCreated response =
    case response of
        NotAsked -> loading


        Loading -> loading


        Failure e -> errorToString e |> errorDisplay


        Success maybeCreatedGame ->
            case maybeCreatedGame of
                Just createdGame ->
                    div []
                    [ h3 [] [ text createdGame.title ]
                    , hr [] []
                    , div [] [ ["Pin: ", createdGame.pin] |> String.concat |> text ]
                    , button [onClick (StartGame createdGame.gameId createdGame.adminCode)] [ text "Start Game" ]
                    ]

                Nothing -> errorDisplay "createdGame not present"





main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
