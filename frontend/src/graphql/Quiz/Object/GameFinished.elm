-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Quiz.Object.GameFinished exposing (..)

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


game :
    SelectionSet decodesTo Quiz.Object.Game
    -> SelectionSet decodesTo Quiz.Object.GameFinished
game object____ =
    Object.selectionForCompositeField "game" [] object____ identity


leaderBoard :
    SelectionSet decodesTo Quiz.Object.LeaderboardEntry
    -> SelectionSet (List decodesTo) Quiz.Object.GameFinished
leaderBoard object____ =
    Object.selectionForCompositeField "leaderBoard" [] object____ (identity >> Decode.list)
