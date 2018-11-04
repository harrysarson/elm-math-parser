module Real.Function exposing (Function(..), fromString, toFunction, toString)

{-| Basic functions to use with expressions.


# Types

-}


{-| A basic function.
-}
type Function
    = NaturalLogarithm
    | SquareRoot
      -- Trigonometric
    | Sine
    | Cosine
    | Tangent
    | ArcSine
    | ArcCosine
    | ArcTangent


fromString : String -> Maybe Function
fromString name =
    case name of
        "log" ->
            Just NaturalLogarithm

        "sqrt" ->
            Just SquareRoot

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


toString : Function -> String
toString func =
    case func of
        NaturalLogarithm ->
            "log"

        SquareRoot ->
            "sqrt"

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


toFunction : Function -> (Float -> Float)
toFunction func =
    case func of
        NaturalLogarithm ->
            logBase e

        SquareRoot ->
            sqrt

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
