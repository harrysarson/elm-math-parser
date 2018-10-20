module MathParserSpec exposing (tests)

import Expect
import Fuzz
import MaFuzz
import MathExpression exposing (MathExpression)
import MathParser exposing (expression)
import MathToString
import ParserError
import String
import Test exposing (describe, fuzz, fuzz2, fuzz3, test)


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
                                MathExpression.BinaryOperation
                                    (MathExpression.Symbol trimmedLhs)
                                    op
                                    (MathExpression.Symbol trimmedRhs)
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
                                                    Debug.todo <|
                                                        "trimmed string "
                                                            ++ Debug.toString trimmed
                                                            ++ " surely must be contained within untrimmed string "
                                                            ++ Debug.toString untrimmed
                                            )
                                        )
                            }
                in
                (lhs ++ MathToString.stringifyBinaryOperator op ++ rhs)
                    |> MathParser.expression (.source >> Just) (always Nothing)
                    |> Expect.equal parseResult
        , fuzz2
            MaFuzz.binaryOperator
            (Fuzz.tuple3
                ( MaFuzz.addSpaces MaFuzz.symbol
                , MaFuzz.addSpaces MaFuzz.symbol
                , MaFuzz.addSpaces MaFuzz.symbol
                )
            )
            "Parse binary operators with left associativity"
          <|
            \op ( a, b, c ) ->
                let
                    expectedAst =
                        Ok <|
                            MathExpression.BinaryOperation
                                (MathExpression.BinaryOperation
                                    (MathExpression.Symbol (String.trim a))
                                    op
                                    (MathExpression.Symbol (String.trim b))
                                )
                                op
                                (MathExpression.Symbol (String.trim c))
                in
                a
                    ++ MathToString.stringifyBinaryOperator op
                    ++ b
                    ++ MathToString.stringifyBinaryOperator op
                    ++ c
                    |> MathParser.expression (.source >> Just) (always Nothing)
                    |> Result.map .expression
                    |> Expect.equal expectedAst
        , fuzz3
            (Fuzz.tuple
                ( MaFuzz.addSpaces MaFuzz.symbol
                , MaFuzz.binaryOperator
                )
            )
            MaFuzz.spaces
            (Fuzz.tuple
                ( MaFuzz.unaryOperator
                , MaFuzz.addSpaces MaFuzz.symbol
                )
            )
            "Parse unary operators with higher precedence than binary operators"
          <|
            \( a, binaryOp ) spaces ( unaryOp, b ) ->
                let
                    expectedAst =
                        Ok <|
                            MathExpression.BinaryOperation
                                (MathExpression.Symbol (String.trim a))
                                binaryOp
                                (MathExpression.UnaryOperation
                                    unaryOp
                                    (MathExpression.Symbol (String.trim b))
                                )
                in
                a
                    ++ MathToString.stringifyBinaryOperator binaryOp
                    ++ spaces
                    ++ MathToString.stringifyUnaryOperator unaryOp
                    ++ b
                    |> MathParser.expression (.source >> Just) (always Nothing)
                    |> Result.map .expression
                    |> Expect.equal expectedAst
        ]
