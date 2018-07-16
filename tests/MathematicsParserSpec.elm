module MathematicsParserSpec exposing (tests)

import Expression exposing (Expression)
import Parser exposing (run)
import MathematicsParser exposing (expression)
import Test exposing (test, describe)
import Expect


tests =
    describe "Parsing binary operators"
        [ test "Adds two numbers" <|
            \() ->
                let
                    expectedAst =
                        Ok <|
                            Expression.BinaryOperator
                                (Expression.Symbol "5")
                                '+'
                                (Expression.Symbol "7")
                in
                    "5+7"
                        |> Parser.run expression
                        |> Expect.equal (expectedAst)
        , test "Adds two numbers with spaces" <|
            \() ->
                let
                    expectedAst =
                        Ok <|
                            Expression.BinaryOperator
                                (Expression.Symbol "5")
                                '+'
                                (Expression.Symbol "7")
                in
                    "   5 +   7        "
                        |> Parser.run expression
                        |> Expect.equal (expectedAst)
        , test "Adds three numbers" <|
            \() ->
                let
                    expectedAst =
                        Ok <|
                            Expression.BinaryOperator
                                (Expression.BinaryOperator
                                    (Expression.Symbol "5")
                                    '+'
                                    (Expression.Symbol "7")
                                )
                                '+'
                                (Expression.Symbol "1")
                in
                    "   5 +   7     + 1   "
                        |> Parser.run expression
                        |> Expect.equal (expectedAst)
        ]
