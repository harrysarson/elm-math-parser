module MathFunctionSpec exposing (tests)

import Expect
import Fuzz
import MaFuzz
import MathFunction
import MathParser exposing (expression)
import MathToString
import ParserError
import String
import Test exposing (describe, fuzz, fuzz2, fuzz3, test)


tests : Test.Test
tests =
    describe "MathFunctions"
        [ fuzz MaFuzz.mathFunction "to and from string round trip" <|
            \func ->
                func
                    |> MathFunction.toString
                    |> MathFunction.fromString
                    |> Expect.equal (Just func)
        ]
