module Real.Evaluator exposing (evaluate)

import MathExpression exposing (MathExpression)


evaluate : MathExpression Float (Float -> Float) -> Float
evaluate expression =
    case expression of
        MathExpression.BinaryOperation lhs op rhs ->
            let
                lhsValue =
                    evaluate lhs

                opFunc =
                    evaluateBinaryOperator op

                rhsValue =
                    evaluate rhs
            in
            opFunc lhsValue rhsValue

        MathExpression.UnaryOperation op rhs ->
            let
                opFunc =
                    evaluateUnaryOperator op

                rhsValue =
                    evaluate rhs
            in
            opFunc rhsValue

        MathExpression.ConjugateTranspose lhs ->
            evaluate lhs

        MathExpression.Parentheses expr ->
            evaluate expr

        MathExpression.Symbol symbol ->
            symbol

        MathExpression.Function function argument ->
            function (evaluate argument)


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
