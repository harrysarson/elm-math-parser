module Expression exposing (Expression(..), binaryOperators, stringify)

{-| This library contains types needed to represent simple mathematical expressions.


# Types

@docs Expression

-}


binaryOperators : List Char
binaryOperators =
    [ '+'
    ]


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
