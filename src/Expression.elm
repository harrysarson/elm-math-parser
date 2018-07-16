module Expression exposing (Expression(..), binaryOperators, isValidSymbolChar, stringify)

{-| This library contains types needed to represent simple mathematical expressions.


# Types

@docs Expression

-}


binaryOperators =
    [ '+'
    , '-'
    ]


{-| A mathematical expression.
-}
type Expression
    = BinaryOperator Expression Char Expression
    | UnaryOperator Char Expression
    | Symbol String


stringify expression =
    case expression of
        BinaryOperator lhs op rhs ->
            "( " ++ stringify lhs ++ " " ++ String.fromChar op ++ " " ++ stringify rhs ++ " )"

        UnaryOperator op expr ->
            "( " ++ String.fromChar op ++ " " ++ stringify expr ++ " )"

        Symbol str ->
            str


isValidSymbolChar charToTest =
    (charToTest /= ' ')
        && List.all (\c -> c /= charToTest) binaryOperators
