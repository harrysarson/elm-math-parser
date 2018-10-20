module MathEvaluatorSpec exposing (tests)

import Expect
import Fuzz
import MaFuzz
import MathEvaluator
import MathExpression
import MathFunction
import MathParser
import MathToString
import ParserError
import String
import Test exposing (describe, fuzz, fuzz2, fuzz3, test)


tests : Test.Test
tests =
    describe "MathEvaluator"
        [ describe "evaluate"
            [ evaluatorTest "6" 6
            , evaluatorTest "(4)" 4
            , evaluatorTest "5 + 5" 10
            , evaluatorTest "5 / 2" 2.5
            , evaluatorTest "7 ^ 2 + 1" 50
            , evaluatorTest "3 * 3 - 3 ^ 2" 0
            , evaluatorTest "sqrt[64]" 8
            , evaluatorTest "4 + (5 / 2)" 6.5
            , evaluatorTest "+5" 5
            , evaluatorTest "-6 / 2" -3
            , evaluatorTest "4.4" 4.4
            , evaluatorTest "123.568e12" 1.23568e14
            , evaluatorTest "4e2'" 4.0e2
            ]
        ]


tolerance =
    Expect.AbsoluteOrRelative 1.0e-10 1.0e-10


type EvaluatorTestError
    = InvalidSymbol
    | InvalidFunctionName


evaluatorTest : String -> Float -> Test.Test
evaluatorTest expression expected =
    test
        (expression ++ " ~= " ++ String.fromFloat expected)
    <|
        \() ->
            expression
                |> MathParser.expression
                |> (\parsedResult ->
                        case parsedResult of
                            Ok parsed ->
                                case
                                    parsed.expression
                                        |> MathExpression.updateSymbols (String.toFloat >> Result.fromMaybe InvalidSymbol)
                                        |> Result.andThen (MathExpression.updateFunctions (MathFunction.fromString >> Result.fromMaybe InvalidFunctionName))
                                        |> Result.map (MathExpression.mapFunctions MathFunction.toRealFunction)
                                of
                                    Ok expr ->
                                        expr
                                            |> MathEvaluator.evaluate
                                            |> Expect.within tolerance expected

                                    Err InvalidSymbol ->
                                        Expect.fail (MathToString.stringifyExpression parsed.expression ++ " contains symbols that cannot be converted to floats")

                                    Err InvalidFunctionName ->
                                        Expect.fail (MathToString.stringifyExpression parsed.expression ++ " contains invalid function names")

                            Err e ->
                                Expect.fail (Debug.toString e)
                   )
