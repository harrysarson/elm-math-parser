module OperatorPrecidenceSpec exposing (tests)

import Expect
import MathParser
import MathToString
import String
import Test exposing (describe, test)


tests : Test.Test
tests =
    describe "Operator precedence"
        [ makePrecedenceTest "7 + 8"
        , makePrecedenceTest "(aA0 - bB1) + cC2"
        , makePrecedenceTest "(((123123 / 12314) + (12313 * 1231241)) - 123) - 1"
        , makePrecedenceTest "(6 / 5) - 1"
        , makePrecedenceTest "(4 * 6) + 2"
        , makePrecedenceTest "2 - (8 / 7)"
        , makePrecedenceTest "2 + (-6)"
        , makePrecedenceTest "(+str) - 19"
        , makePrecedenceTest "(8 * (-2)) - (STR / 7)"
        , makePrecedenceTest "2 - (4 / (-8))"
        , makePrecedenceTest "(5 ^ 2) - (4 / (-8))"
        , makePrecedenceTest "a + ((3 / (5 ^ 3)) * 8)"
        , makePrecedenceTest "x - (y')"
        , makePrecedenceTest "jkasn / ((65 ^ faifinfinfin)')"
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
                |> MathParser.expression
                |> Result.map .expression
                |> Result.map MathToString.stringifyExpression
                |> Expect.equal (Ok withParenthesis)
