module MathEvaluator exposing (EvaluationError, evaluate)

import MathExpression exposing (MathExpression)


type EvaluationError
    = NonNumericSymbol String


evaluate : (f -> (Float -> Float)) -> MathExpression f -> Result EvaluationError Float
evaluate getRealFunction expression =
    case expression of
        MathExpression.BinaryOperation lhs op rhs ->
            let
                lhsValue =
                    evaluate getRealFunction lhs

                opFunc =
                    evaluateBinaryOperator op

                rhsValue =
                    evaluate getRealFunction rhs
            in
            Result.map2 opFunc lhsValue rhsValue

        MathExpression.UnaryOperation op rhs ->
            let
                opFunc =
                    evaluateUnaryOperator op

                rhsValue =
                    evaluate getRealFunction rhs
            in
            Result.map opFunc rhsValue

        MathExpression.ConjugateTranspose lhs ->
            evaluate getRealFunction lhs

        MathExpression.Parentheses expr ->
            evaluate getRealFunction expr

        MathExpression.Symbol symbol ->
            String.toFloat symbol
                |> Result.fromMaybe (NonNumericSymbol symbol)

        MathExpression.Function function argument ->
            evaluate getRealFunction argument
                |> Result.map (getRealFunction function)


evaluateBinaryOperator : MathExpression.BinaryOperator -> Float -> Float -> Float
evaluateBinaryOperator op =
    case op of
        MathExpression.Add ->
            (+)

        MathExpression.Subtract ->
            (-)

        MathExpression.Multiply ->
            (*)

        MathExpression.Divide ->
            (/)

        MathExpression.Exponentiate ->
            (^)


evaluateUnaryOperator : MathExpression.UnaryOperator -> Float -> Float
evaluateUnaryOperator op =
    case op of
        MathExpression.UnaryAdd ->
            identity

        MathExpression.UnarySubtract ->
            (-) 0
