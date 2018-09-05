module MathToString exposing (stringifyExpression, stringifyBinaryOperator, stringifyUnaryOperator)

{-| This module allows expressions to be represented as strings.


# Output

@docs stringifyExpression, stringifyBinaryOperator, stringifyUnaryOperator

-}

import MathExpression exposing (..)


stringifyWithParentheses : (f -> String) -> MathExpression f -> String
stringifyWithParentheses functionToString expression =
    case expression of
        Symbol _ ->
            stringifyExpression functionToString expression

        _ ->
            "( " ++ stringifyExpression functionToString expression ++ " )"


{-| Display a binary operator in string form.
-}
stringifyBinaryOperator : BinaryOperator -> String
stringifyBinaryOperator op =
    case op of
        Add ->
            "+"

        Subtract ->
            "-"

        Multiply ->
            "*"

        Divide ->
            "/"


{-| Display an unary operator in string form.
-}
stringifyUnaryOperator : UnaryOperator -> String
stringifyUnaryOperator op =
    case op of
        UnaryAdd ->
            "+"

        UnarySubtract ->
            "-"


{-| Display an expression in string form.
-}
stringifyExpression : (f -> String) -> MathExpression f -> String
stringifyExpression functionToString expression =
    case expression of
        BinaryOperation lhs op rhs ->
            stringifyWithParentheses functionToString lhs
                ++ " "
                ++ stringifyBinaryOperator op
                ++ " "
                ++ stringifyWithParentheses functionToString rhs

        UnaryOperation op expr ->
            stringifyUnaryOperator op ++ " " ++ stringifyWithParentheses functionToString expr

        Parentheses expr ->
            stringifyWithParentheses functionToString expr

        Symbol str ->
            str

        Function f args ->
            functionToString f ++ "[" ++ stringifyExpression functionToString args ++ "]"
