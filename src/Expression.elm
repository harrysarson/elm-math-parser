module Expression exposing (Expression(..), binaryOperators, isValidSymbolChar)

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


isValidSymbolChar charToTest =
    (charToTest /= ' ')
        && List.all (\c -> c /= charToTest) binaryOperators
