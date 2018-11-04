module Complex.Evaluator exposing (evaluate)

import Complex exposing (Complex)
import MathExpression exposing (MathExpression)


evaluate : MathExpression Complex (Complex -> Complex) -> Complex
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
            Complex.conjugate (evaluate lhs)

        MathExpression.Parentheses expr ->
            evaluate expr

        MathExpression.Symbol symbol ->
            symbol

        MathExpression.Function function argument ->
            function (evaluate argument)


evaluateBinaryOperator : MathExpression.BinaryOperator -> Complex -> Complex -> Complex
evaluateBinaryOperator op =
    case op of
        MathExpression.Add ->
            Complex.add

        MathExpression.Subtract ->
            Complex.subtract

        MathExpression.Multiply ->
            Complex.multiply

        MathExpression.Divide ->
            Complex.divide

        MathExpression.Exponentiate ->
            Complex.pow


evaluateUnaryOperator : MathExpression.UnaryOperator -> Complex -> Complex
evaluateUnaryOperator op =
    case op of
        MathExpression.UnaryAdd ->
            identity

        MathExpression.UnarySubtract ->
            Complex.subtract Complex.zero
