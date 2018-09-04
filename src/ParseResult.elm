module ParseResult exposing (ParseResult)

import Dict exposing (Dict)
import Expression exposing (Expression)
import Set exposing (Set)


type alias ParseResult =
    { expression : Expression
    , symbols : List ( String, Int )
    }
