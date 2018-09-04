module ParseResult exposing (ParseResult)

import Dict exposing (Dict)
import MathExpression exposing (MathExpression)
import Set exposing (Set)


type alias ParseResult =
    { expression : MathExpression
    , symbols : List ( String, Int )
    }
