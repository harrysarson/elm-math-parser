module Real.FunctionSpec exposing (tests)

import Expect
import Fuzz
import MathFuzz
import MathParser exposing (expression)
import MathToString
import ParserError
import Real.Function
import String
import Test exposing (describe, fuzz, fuzz2, fuzz3, test)


tests : Test.Test
tests =
    describe "Real Functions"
        [ fuzz MathFuzz.realFunction "to and from string round trip" <|
            \func ->
                func
                    |> Real.Function.toString
                    |> Real.Function.fromString
                    |> Expect.equal (Just func)
        ]
