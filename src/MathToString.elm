module MathToString exposing (stringifyExpression, stringifyBinaryOperator, stringifyUnaryOperator)

{-| This module allows expressions to be represented as strings.


# Output

@docs stringifyExpression, stringifyBinaryOperator, stringifyUnaryOperator

-}

import MathExpression exposing (..)


stringifyWithParentheses : MathExpression -> String
stringifyWithParentheses expression =
    case expression of
        Symbol _ ->
            stringifyExpression expression

        _ ->
            "( " ++ stringifyExpression expression ++ " )"


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
stringifyExpression : MathExpression -> String
stringifyExpression expression =
    case expression of
        BinaryOperation lhs op rhs ->
            stringifyWithParentheses lhs ++ " " ++ stringifyBinaryOperator op ++ " " ++ stringifyWithParentheses rhs

        UnaryOperation op expr ->
            stringifyUnaryOperator op ++ " " ++ stringifyWithParentheses expr

        Parentheses expr ->
            stringifyWithParentheses expr

        Symbol str ->
            str
