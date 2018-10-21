module ErrorsSpec exposing (tests)

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
    describe "Parsing Errors"
        [ describe "Empty strings"
            [ test "Empty expression" <|
                \() -> errorExpect "" 0 ParserError.EmptyString
            , test "Empty parentheses" <|
                \() -> errorExpect "() - 2" 1 ParserError.EmptyParentheses
            , test "Empty parentheses in middle of expression" <|
                \() -> errorExpect "a * () + 3" 5 ParserError.EmptyParentheses
            ]
        , describe "Invalid characters"
            [ test "expression containing a non valid symbol character" <|
                \() -> errorExpect "a&b" 1 (ParserError.InvalidChar '&')
            , test "binary operation with a non valid symbol character lhs and no rhs" <|
                \() -> errorExpect "& +" 0 (ParserError.InvalidChar '&')
            , test "binary operation with a non valid symbol character lhs" <|
                \() -> errorExpect "£ + 3" 0 (ParserError.InvalidChar '£')
            , test "binary operation with a non valid symbol character rhs" <|
                \() -> errorExpect "3 + %" 4 (ParserError.InvalidChar '%')
            , test "binary operation with a non valid symbol character lhs and unclosed parenthesis rhs" <|
                \() -> errorExpect "$ + (" 0 (ParserError.InvalidChar '$')
            ]
        , describe "Missing operands"
            [ test "Just a unary operator" <|
                \() -> errorExpect "+" 0 ParserError.MissingUnaryOperand
            , test "Just a binary operator" <|
                \() -> errorExpect "*" 0 (ParserError.MissingBinaryOperand ParserError.LeftHandSide)
            , test "Just a conjugate transpose" <|
                \() -> errorExpect "'" 0 ParserError.MissingConjugateTransposeOperand
            , test "Binary operator with only lhs" <|
                \() -> errorExpect "5 *" 2 (ParserError.MissingBinaryOperand ParserError.RightHandSide)
            , test "Binary operator with only rhs" <|
                \() -> errorExpect "/ 3" 0 (ParserError.MissingBinaryOperand ParserError.LeftHandSide)
            , test "Binary/unary operator with only lhs" <|
                \() -> errorExpect "5 +" 2 (ParserError.MissingBinaryOperand ParserError.RightHandSide)
            , test "Tranpose operator missing operand" <|
                \() -> errorExpect "5 * ' + 2" 4 ParserError.MissingConjugateTransposeOperand
            ]
        , describe "Ambiguous operator combinations"
            [ test "Unary operator as left hand side of exponential" <|
                \() ->
                    MathParser.expression "-2^x"
                        |> Result.mapError .position
                        |> Expect.equal (Err 0)
            , test "Repeated exponentiation" <|
                \() ->
                    MathParser.expression "2 ^ 5 ^ 88"
                        |> Result.mapError .position
                        |> Expect.equal (Err 5)
            ]
        ]


errorExpect string position errorType =
    MathParser.expression string
        |> Expect.all
            [ Result.mapError .errorType
                >> Expect.equal (Err errorType)
            , Result.mapError .position
                >> Expect.equal (Err position)
            ]



{-
   # errors to write tests for
      - input: "("
        errorType: MissingClosingParenthesis
        position: 1
        source: "("
      - input: "( 4"
        errorType: MissingClosingParenthesis
        position: 3
        source: "( 4"
      - input: "5 / ("
        errorType: MissingClosingParenthesis
        position: 4
        source: "("
      - input: "2 * ( 5 +"
        errorType: MissingClosingParenthesis
        position: 9
        source: "( 5 +"
      - input: "( &"
        errorType: MissingClosingParenthesis
        position: 3
        source: "( &"
      - input: "2 + ( 1 / (2 + 3) + 7"
        errorType: MissingClosingParenthesis
        position: 0
        source: "&"
-}
