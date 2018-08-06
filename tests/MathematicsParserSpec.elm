module MathematicsParserSpec exposing (tests)

import String
import Expression exposing (Expression)
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
                    trimmedLhs =
                        String.trim lhs

                    trimmedRhs =
                        String.trim rhs

                    parseResult =
                        Ok <|
                            { expression =
                                Expression.BinaryOperator
                                    (Expression.Symbol trimmedLhs)
                                    op
                                    (Expression.Symbol trimmedRhs)
                            , symbols =
                                [ ( trimmedLhs, lhs, 0 )
                                , ( trimmedRhs, rhs, String.length lhs + 1 )
                                ]
                                    |> List.map
                                        (\( trimmed, untrimmed, add ) ->
                                            ( trimmed
                                            , case
                                                untrimmed
                                                    |> String.indexes trimmed
                                                    |> List.head
                                              of
                                                Just index ->
                                                    index + add

                                                Nothing ->
                                                    Debug.crash <| "trimmed string " ++ toString trimmed ++ " surely must be contained within untrimmed string " ++ toString untrimmed
                                            )
                                        )
                            }
                in
                    (lhs ++ String.fromChar op ++ rhs)
                        |> expression
                        |> Expect.equal parseResult
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
                        |> expression
                        |> Result.map .expression
                        |> Expect.equal expectedAst
        , fuzz
            (Fuzz.tuple5
                ( MaFuzz.addSpaces MaFuzz.symbol
                , MaFuzz.binaryOperator
                , MaFuzz.spaces
                , MaFuzz.unaryOperator
                , MaFuzz.addSpaces MaFuzz.symbol
                )
            )
            "Parse unary operators with higher precedence than binary operators"
          <|
            \( a, binaryOp, spaces, unaryOp, b ) ->
                let
                    expectedAst =
                        Ok <|
                            Expression.BinaryOperator
                                (Expression.Symbol (String.trim a))
                                binaryOp
                                (Expression.UnaryOperator
                                    unaryOp
                                    (Expression.Symbol (String.trim b))
                                )
                in
                    a
                        ++ String.fromChar binaryOp
                        ++ spaces
                        ++ String.fromChar unaryOp
                        ++ b
                        |> expression
                        |> Result.map .expression
                        |> Expect.equal expectedAst
        , describe "Operator precedence"
            [ makePrecedenceTest "7 + 8"
            , makePrecedenceTest "( aA0 - bB1 ) + cC2"
            , makePrecedenceTest "( ( ( 123123 / 12314 ) + ( 12313 * 1231241 ) ) - 123 ) - 1"
            , makePrecedenceTest "( 6 / 5 ) - 1"
            , makePrecedenceTest "( 4 * 6 ) + 2"
            , makePrecedenceTest "2 - ( 8 / 7 )"
            , makePrecedenceTest "2 + ( - 6 )"
            , makePrecedenceTest "( + str ) - 19"
            , makePrecedenceTest "( 8 * ( - 2 ) ) - ( STR / 7 )"
            , makePrecedenceTest "2 - ( 4 / ( - 8 ) )"
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
                    |> expression
                    |> Result.map .expression
                    |> Result.map Expression.stringify
                    |> Expect.equal (Ok withParenthesis)
