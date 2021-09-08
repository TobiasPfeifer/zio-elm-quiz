-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Quiz.Object.Question exposing (..)

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


question : SelectionSet String Quiz.Object.Question
question =
    Object.selectionForField "String" "question" [] Decode.string


imageUrl : SelectionSet (Maybe String) Quiz.Object.Question
imageUrl =
    Object.selectionForField "(Maybe String)" "imageUrl" [] (Decode.string |> Decode.nullable)


points : SelectionSet Int Quiz.Object.Question
points =
    Object.selectionForField "Int" "points" [] Decode.int


choices :
    SelectionSet decodesTo Quiz.Object.Choices
    -> SelectionSet decodesTo Quiz.Object.Question
choices object____ =
    Object.selectionForCompositeField "choices" [] object____ identity
