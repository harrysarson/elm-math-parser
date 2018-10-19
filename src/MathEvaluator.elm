module MathEvaluator exposing (EvaluationError, evaluate, evaluateWithScope)

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


evaluateWithScope : (f -> (Float -> Float)) -> (String -> Maybe Float) -> MathExpression f -> Result EvaluationError Float
evaluateWithScope getRealFunction scope expression =
    case expression of
        MathExpression.BinaryOperation lhs op rhs ->
            let
                lhsValue =
                    evaluateWithScope getRealFunction scope lhs

                opFunc =
                    evaluateBinaryOperator op

                rhsValue =
                    evaluateWithScope getRealFunction scope rhs
            in
            Result.map2 opFunc lhsValue rhsValue

        MathExpression.UnaryOperation op rhs ->
            let
                opFunc =
                    evaluateUnaryOperator op

                rhsValue =
                    evaluateWithScope getRealFunction scope rhs
            in
            Result.map opFunc rhsValue

        MathExpression.ConjugateTranspose lhs ->
            evaluateWithScope getRealFunction scope lhs

        MathExpression.Parentheses expr ->
            evaluateWithScope getRealFunction scope expr

        MathExpression.Symbol symbol ->
            case scope symbol of
                Just float ->
                    Ok float

                Nothing ->
                    String.toFloat symbol
                        |> Result.fromMaybe (NonNumericSymbol symbol)

        MathExpression.Function function argument ->
            evaluateWithScope getRealFunction scope argument
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
