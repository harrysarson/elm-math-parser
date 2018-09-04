module MathExpression exposing
    ( MathExpression(..)
    , binaryOperators, unaryOperators
    , stringify
    )

{-| This library contains types needed to represent simple mathematical expressions.


# Types

@docs MathExpression


# Definitions

@docs binaryOperators, unaryOperators


# Output

@docs stringify

-}

import Set exposing (Set)


binaryOperators : List (Set Char)
binaryOperators =
    [ [ '+', '-' ]
    , [ '*', '/' ]
    ]
        |> List.map Set.fromList


unaryOperators : Set Char
unaryOperators =
    [ '+', '-' ]
        |> Set.fromList


{-| A mathematical expression.
-}
type MathExpression
    = BinaryOperator MathExpression Char MathExpression
    | UnaryOperator Char MathExpression
    | Parentheses MathExpression
    | Symbol String


stringifyWithParentheses : MathExpression -> String
stringifyWithParentheses expression =
    case expression of
        Symbol _ ->
            stringify expression

        _ ->
            "( " ++ stringify expression ++ " )"


stringify : MathExpression -> String
stringify expression =
    case expression of
        BinaryOperator lhs op rhs ->
            stringifyWithParentheses lhs ++ " " ++ String.fromChar op ++ " " ++ stringifyWithParentheses rhs

        UnaryOperator op expr ->
            String.fromChar op ++ " " ++ stringifyWithParentheses expr

        Parentheses expr ->
            stringifyWithParentheses expr

        Symbol str ->
            str
