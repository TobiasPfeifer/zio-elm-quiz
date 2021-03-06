-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Quiz.Object.QuizEntity exposing (..)

import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode
import Quiz.InputObject
import Quiz.Interface
import Quiz.Object
import Quiz.Scalar
import Quiz.ScalarCodecs
import Quiz.Union


quizId : SelectionSet String Quiz.Object.QuizEntity
quizId =
    Object.selectionForField "String" "quizId" [] Decode.string


quiz :
    SelectionSet decodesTo Quiz.Object.Quiz
    -> SelectionSet decodesTo Quiz.Object.QuizEntity
quiz object____ =
    Object.selectionForCompositeField "quiz" [] object____ identity
