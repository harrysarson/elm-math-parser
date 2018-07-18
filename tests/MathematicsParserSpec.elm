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
        , describe "Operator precidence"
            [ makePrecedenceTest "( 7 + 8 )"
            , makePrecedenceTest "( ( aA0 - bB1 ) + cC2 )"
            , makePrecedenceTest "( ( ( ( 123123 / 12314 ) + ( 12313 * 1231241 ) ) - 123 ) - 1 )"
            , makePrecedenceTest "( ( 6 / 5 ) - 1 )"
            , makePrecedenceTest "( ( 4 * 6 ) + 2 )"
            , makePrecedenceTest "( 2 - ( 8 / 7 ) )"
            ]
        ]


makePrecedenceTest : String -> Test.Test
makePrecedenceTest withParenthesis =
    let
        withoutParenthesis =
            withParenthesis
                |> String.split "( "
                |> String.join ""
                |> String.split " )"
                |> String.join ""
    in
        test
            (withoutParenthesis ++ " == " ++ withParenthesis)
        <|
            \() ->
                withoutParenthesis
                    |> Parser.run expression
                    |> Result.map Expression.stringify
                    |> Expect.equal (Ok withParenthesis)
