module MathematicsParserSpec exposing (tests)

import String
import Expression exposing (Expression)
import Parser exposing (run)
import MathematicsParser exposing (expression)
import Test exposing (test, describe, fuzz)
import Fuzz
import Expect
import MaFuzz


tests : Test.Test
tests =
    describe "Parse mathematical expressions"
        [ fuzz
            (Fuzz.tuple3
                ( MaFuzz.addSpaces MaFuzz.symbol
                , MaFuzz.binaryOperator
                , MaFuzz.addSpaces MaFuzz.symbol
                )
            )
            "Parse binary operators"
          <|
            \( lhs, op, rhs ) ->
                let
                    expectedAst =
                        Ok <|
                            Expression.BinaryOperator
                                (Expression.Symbol (String.trim lhs))
                                op
                                (Expression.Symbol (String.trim rhs))
                in
                    (lhs ++ String.fromChar op ++ rhs)
                        |> Parser.run expression
                        |> Expect.equal (expectedAst)
        , fuzz
            (Fuzz.tuple4
                ( MaFuzz.binaryOperator
                , MaFuzz.addSpaces MaFuzz.symbol
                , MaFuzz.addSpaces MaFuzz.symbol
                , MaFuzz.addSpaces MaFuzz.symbol
                )
            )
            "Parse binary operators with left associativity"
          <|
            \( op, a, b, c ) ->
                let
                    expectedAst =
                        Ok <|
                            Expression.BinaryOperator
                                (Expression.BinaryOperator
                                    (Expression.Symbol (String.trim a))
                                    op
                                    (Expression.Symbol (String.trim b))
                                )
                                op
                                (Expression.Symbol (String.trim c))
                in
                    a
                        ++ String.fromChar op
                        ++ b
                        ++ String.fromChar op
                        ++ c
                        |> Parser.run expression
                        |> Expect.equal (expectedAst)
        ]
