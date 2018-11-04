module Complex.Function exposing (Function(..), fromString, toFunction, toString)

{-| Basic functions to use with expressions containing complex numbers.


# Types

@doc Function


# conversion

@doc fromString, toString


# evaluate


# toFunction

-}

import Complex exposing (Complex)


{-| A basic function.
-}
type Function
    = NaturalLogarithm
    | SquareRoot


fromString : String -> Maybe Function
fromString name =
    case name of
        "log" ->
            Just NaturalLogarithm

        "sqrt" ->
            Just SquareRoot

        _ ->
            Nothing


toString : Function -> String
toString func =
    case func of
        NaturalLogarithm ->
            "log"

        SquareRoot ->
            "sqrt"


toFunction : Function -> (Complex -> Complex)
toFunction func =
    case func of
        NaturalLogarithm ->
            Complex.log

        SquareRoot ->
            \c -> Complex.pow c (Complex.real 0.5)
