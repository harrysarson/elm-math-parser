module ParseResult exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)
import Expression exposing (Expression)


type alias ParseResult =
    { expression : Expression
    , symbols : List ( String, Int )
    }
