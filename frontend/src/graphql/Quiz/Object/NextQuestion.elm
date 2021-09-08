-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Quiz.Object.NextQuestion exposing (..)

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


question :
    SelectionSet decodesTo Quiz.Object.Question
    -> SelectionSet decodesTo Quiz.Object.NextQuestion
question object____ =
    Object.selectionForCompositeField "question" [] object____ identity
