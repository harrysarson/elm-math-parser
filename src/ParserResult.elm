module ParserResult exposing (ParserResult)

import Dict exposing (Dict)
import MathExpression exposing (MathExpression)
import Set exposing (Set)


type alias ParserResult f =
    { expression : MathExpression f
    , symbols : List ( String, Int )
    }
