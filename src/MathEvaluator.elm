module MathEvaluator exposing (evaluate, evaluateWithScope)

import MathExpression exposing (MathExpression)


evaluate : (f -> (Float -> Float)) -> MathExpression Float f -> Float
evaluate getRealFunction expression =
    evaluateWithScope identity getRealFunction expression


evaluateWithScope : (s -> Float) -> (f -> (Float -> Float)) -> MathExpression s f -> Float
evaluateWithScope scope getRealFunction expression =
    case expression of
        MathExpression.BinaryOperation lhs op rhs ->
            let
                lhsValue =
                    evaluateWithScope scope getRealFunction lhs

                opFunc =
                    evaluateBinaryOperator op

                rhsValue =
                    evaluateWithScope scope getRealFunction rhs
            in
            opFunc lhsValue rhsValue

        MathExpression.UnaryOperation op rhs ->
            let
                opFunc =
                    evaluateUnaryOperator op

                rhsValue =
                    evaluateWithScope scope getRealFunction rhs
            in
            opFunc rhsValue

        MathExpression.ConjugateTranspose lhs ->
            evaluateWithScope scope getRealFunction lhs

        MathExpression.Parentheses expr ->
            evaluateWithScope scope getRealFunction expr

        MathExpression.Symbol symbol ->
            scope symbol

        MathExpression.Function function argument ->
            evaluateWithScope scope getRealFunction argument
                |> getRealFunction function


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
