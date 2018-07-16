module Expression exposing (Expression(..), binaryOperators, isValidSymbolChar, stringify)

{-| This library contains types needed to represent simple mathematical expressions.


# Types

@docs Expression

-}

import Char


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
    let
        testInRange lower upper char =
            let
                lowerNum =
                    Char.toCode lower

                upperNum =
                    Char.toCode upper

                charNum =
                    Char.toCode char
            in
                charNum >= lowerNum && charNum <= upperNum

        isNumber =
            testInRange '0' '9'

        isLowerEnglish =
            testInRange 'a' 'z'

        isUpperEnglish =
            testInRange 'A' 'Z'
    in
        isNumber charToTest
            || isLowerEnglish charToTest
            || isUpperEnglish charToTest
