module MathToStringSpec exposing (tests)

import Expect
import MathEvaluator
import MathFunction
import MathParser
import MathToString
import Test exposing (describe, fuzz, fuzz2, fuzz3, test)


tests : Test.Test
tests =
    describe "MathToString"
        [ describe "evaluate"
            [ mathToStringTest "6"
            , mathToStringTest "5 + 5"
            , mathToStringTest "5 / 2"
            , mathToStringTest "(7 ^ 2) + 1"
            , mathToStringTest "(3 * 3) - (3 ^ 2)"
            , mathToStringTest "sqrt[64]"
            , mathToStringTest "4 + (5 / 2)"
            , mathToStringTest "+5"
            , mathToStringTest "(-6) / 2"
            , mathToStringTest "4.4"
            , mathToStringTest "123.568e12"
            ]
        ]


mathToStringTest : String -> Test.Test
mathToStringTest expression =
    test expression <|
        \() ->
            expression
                |> MathParser.expression MathFunction.fromString
                |> Result.map
                    (.expression
                        >> MathToString.stringifyExpression MathFunction.toString
                    )
                |> Expect.all
                    [ Expect.equal (Ok expression)
                    , Result.map
                        (MathParser.expression MathFunction.fromString
                            >> Result.map
                                (.expression
                                    >> MathToString.stringifyExpression MathFunction.toString
                                )
                        )
                        >> Expect.equal (Ok <| Ok expression)
                    ]