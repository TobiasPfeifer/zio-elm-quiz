-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Quiz.Scalar exposing (Codecs, Long(..), Unit(..), defaultCodecs, defineCodecs, unwrapCodecs, unwrapEncoder)

import Graphql.Codec exposing (Codec)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type Long
    = Long String


type Unit
    = Unit String


defineCodecs :
    { codecLong : Codec valueLong
    , codecUnit : Codec valueUnit
    }
    -> Codecs valueLong valueUnit
defineCodecs definitions =
    Codecs definitions


unwrapCodecs :
    Codecs valueLong valueUnit
    ->
        { codecLong : Codec valueLong
        , codecUnit : Codec valueUnit
        }
unwrapCodecs (Codecs unwrappedCodecs) =
    unwrappedCodecs


unwrapEncoder :
    (RawCodecs valueLong valueUnit -> Codec getterValue)
    -> Codecs valueLong valueUnit
    -> getterValue
    -> Graphql.Internal.Encode.Value
unwrapEncoder getter (Codecs unwrappedCodecs) =
    (unwrappedCodecs |> getter |> .encoder) >> Graphql.Internal.Encode.fromJson


type Codecs valueLong valueUnit
    = Codecs (RawCodecs valueLong valueUnit)


type alias RawCodecs valueLong valueUnit =
    { codecLong : Codec valueLong
    , codecUnit : Codec valueUnit
    }


defaultCodecs : RawCodecs Long Unit
defaultCodecs =
    { codecLong =
        { encoder = \(Long raw) -> Encode.string raw
        , decoder = Object.scalarDecoder |> Decode.map Long
        }
    , codecUnit =
        { encoder = \(Unit raw) -> Encode.string raw
        , decoder = Object.scalarDecoder |> Decode.map Unit
        }
    }