module MathToString exposing (stringifyExpression, stringifyBinaryOperator, stringifyUnaryOperator)

{-| This module allows expressions to be represented as strings.


# Output

@docs stringifyExpression, stringifyBinaryOperator, stringifyUnaryOperator

-}

import MathExpression exposing (..)


stringifyWithParentheses : (s -> String) -> (f -> String) -> MathExpression s f -> String
stringifyWithParentheses symbolToString functionToString expression =
    case expression of
        Symbol _ ->
            stringifyExpression symbolToString functionToString expression

        Function _ _ ->
            stringifyExpression symbolToString functionToString expression

        _ ->
            "(" ++ stringifyExpression symbolToString functionToString expression ++ ")"


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

        Exponentiate ->
            "^"


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
stringifyExpression : (s -> String) -> (f -> String) -> MathExpression s f -> String
stringifyExpression symbolToString functionToString expression =
    case expression of
        BinaryOperation lhs op rhs ->
            stringifyWithParentheses symbolToString functionToString lhs
                ++ " "
                ++ stringifyBinaryOperator op
                ++ " "
                ++ stringifyWithParentheses symbolToString functionToString rhs

        UnaryOperation op expr ->
            stringifyUnaryOperator op ++ stringifyWithParentheses symbolToString functionToString expr

        ConjugateTranspose lhs ->
            stringifyWithParentheses symbolToString functionToString lhs ++ "'"

        Parentheses expr ->
            stringifyWithParentheses symbolToString functionToString expr

        Symbol str ->
            symbolToString str

        Function f args ->
            functionToString f ++ "[" ++ stringifyExpression symbolToString functionToString args ++ "]"
