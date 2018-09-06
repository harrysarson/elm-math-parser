module MathFunction exposing (MathFunction(..), fromString, toString, toRealFunction)

{-| Basic functions to use with expressions.


# Types

-}


{-| A basic function.
-}
type MathFunction
    = NaturalLogarithm


fromString : String -> Maybe MathFunction
fromString name =
    case name of
        "log" ->
            Just NaturalLogarithm

        _ ->
            Nothing


toString : MathFunction -> String
toString func =
    case func of
        NaturalLogarithm ->
            "log"


toRealFunction : MathFunction -> (Float -> Float)
toRealFunction func =
    case func of
        NaturalLogarithm ->
            logBase e
