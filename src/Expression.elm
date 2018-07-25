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
    | Symbol String


stringify : Expression -> String
stringify expression =
    case expression of
        BinaryOperator lhs op rhs ->
            "( " ++ stringify lhs ++ " " ++ String.fromChar op ++ " " ++ stringify rhs ++ " )"

        UnaryOperator op expr ->
            "( " ++ String.fromChar op ++ " " ++ stringify expr ++ " )"

        Symbol str ->
            str
