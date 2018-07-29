module Expression exposing (..)

{-| This library contains types needed to represent simple mathematical expressions.


# Types

@docs Expression

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
type Expression
    = BinaryOperator Expression Char Expression
    | UnaryOperator Char Expression
    | Parentheses Expression
    | Symbol String


stringify : Expression -> String
stringify expression =
    let
        stringifyWithParentheses expression =
            case expression of
                Symbol _ ->
                    stringify expression

                _ ->
                    "( " ++ stringify expression ++ " )"
    in
        case expression of
            BinaryOperator lhs op rhs ->
                stringifyWithParentheses lhs ++ " " ++ String.fromChar op ++ " " ++ stringifyWithParentheses rhs

            UnaryOperator op expr ->
                String.fromChar op ++ " " ++ stringifyWithParentheses expr

            Parentheses expr ->
                stringifyWithParentheses expr

            Symbol str ->
                str
