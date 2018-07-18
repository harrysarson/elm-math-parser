module MathematicsParser exposing (MathematicsParser, expression)

{-| This library allows simple mathematical expressions to be parsed in elm.
A string expression is converted into an abstract syntax tree.


# Mathematics Parsers

@docs MathematicsParser#
@docs expression

-}

import Char
import Set exposing (Set)
import Parser exposing (Parser, (|.), (|=))
import Expression exposing (Expression, binaryOperators)


{-| A mathematical expression.
-}
type alias MathematicsParser =
    Parser Expression


{-| Parse expressions.
-}
expression : Parser Expression
expression =
    List.foldr
        (\opChars nextParser -> operators opChars nextParser)
        symbolParser
        Expression.binaryOperators
        |. Parser.end



-- Parsers --


operators : Set Char -> Parser Expression -> Parser Expression
operators opChars nextParser =
    Parser.inContext ("binary operators " ++ String.join ", " (Set.toList <| Set.map String.fromChar opChars)) <|
        Parser.oneOf
            [ Parser.delayedCommitMap toBinaryOp (spaceIgnoringParser nextParser) <|
                Parser.succeed identity
                    |= Parser.repeat Parser.oneOrMore
                        (Parser.succeed (,)
                            |= Parser.oneOf
                                (List.map
                                    (\c ->
                                        Parser.symbol (String.fromChar c)
                                            |> Parser.map (always c)
                                    )
                                    (Set.toList opChars)
                                )
                            |= spaceIgnoringParser nextParser
                        )
            , spaceIgnoringParser nextParser
            ]


spaceIgnoringParser : Parser a -> Parser a
spaceIgnoringParser parser =
    Parser.succeed identity
        |. spaces
        |= parser
        |. spaces


symbolParser : Parser Expression
symbolParser =
    Parser.inContext "symbol" <|
        Parser.succeed Expression.Symbol
            |= Parser.keep Parser.oneOrMore isValidSymbolChar


spaces : Parser ()
spaces =
    Parser.ignore Parser.zeroOrMore (\c -> c == ' ')



-- Helper Functions --


toBinaryOp : Expression -> List ( Char, Expression ) -> Expression
toBinaryOp expression rhsExtraList =
    case rhsExtraList of
        [] ->
            expression

        ( op, nextRhs ) :: futureRhs ->
            toBinaryOp (Expression.BinaryOperator expression op nextRhs) futureRhs


isValidSymbolChar : Char -> Bool
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
