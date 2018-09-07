module MathFunction exposing (MathFunction(..), fromString, toRealFunction, toString)

{-| Basic functions to use with expressions.


# Types

-}


{-| A basic function.
-}
type MathFunction
    = NaturalLogarithm
      -- Trigonometric
    | Sine
    | Cosine
    | Tangent
    | ArcSine
    | ArcCosine
    | ArcTangent


fromString : String -> Maybe MathFunction
fromString name =
    case name of
        "log" ->
            Just NaturalLogarithm

        "sin" ->
            Just Sine

        "cos" ->
            Just Cosine

        "tan" ->
            Just Tangent

        "asin" ->
            Just ArcSine

        "acos" ->
            Just ArcCosine

        "atan" ->
            Just ArcTangent

        _ ->
            Nothing


toString : MathFunction -> String
toString func =
    case func of
        NaturalLogarithm ->
            "log"

        Sine ->
            "sin"

        Cosine ->
            "cos"

        Tangent ->
            "tan"

        ArcSine ->
            "asin"

        ArcCosine ->
            "acos"

        ArcTangent ->
            "atan"


toRealFunction : MathFunction -> (Float -> Float)
toRealFunction func =
    case func of
        NaturalLogarithm ->
            logBase e

        Sine ->
            sin

        Cosine ->
            cos

        Tangent ->
            tan

        ArcSine ->
            asin

        ArcCosine ->
            acos

        ArcTangent ->
            atan
