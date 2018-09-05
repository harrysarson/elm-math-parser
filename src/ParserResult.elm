module PaserResult exposing (PaserResult)

import Dict exposing (Dict)
import MathExpression exposing (MathExpression)
import Set exposing (Set)


type alias PaserResult =
    { expression : MathExpression
    , symbols : List ( String, Int )
    }
