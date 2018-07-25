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
        (\opChars nextParser -> binaryOperators opChars nextParser)
        (unaryOperators Expression.unaryOperators symbolParser)
        Expression.binaryOperators
        |. Parser.end



-- Parsers --


binaryOperators : Set Char -> Parser Expression -> Parser Expression
binaryOperators opChars nextParser =
    Parser.inContext ("binary operators " ++ String.join ", " (Set.toList <| Set.map String.fromChar opChars)) <|
        Parser.oneOf
            [ Parser.delayedCommitMap toBinaryOp (spaceIgnoringParser nextParser) <|
                Parser.succeed identity
                    |= Parser.repeat Parser.oneOrMore
                        (Parser.succeed (,)
                            |= operatorSymbol opChars
                            |= spaceIgnoringParser nextParser
                        )
            , spaceIgnoringParser nextParser
            ]


unaryOperators : Set Char -> Parser Expression -> Parser Expression
unaryOperators opChars nextParser =
    let
        checkNotAnotherUnary =
            Parser.oneOf
                [ Parser.succeed (Expression.Symbol "")
                    |. spaceIgnoringParser (operatorSymbol opChars)
                    |. Parser.fail "Cannot have two unary operators one after the other"
                , nextParser
                ]
    in
        Parser.inContext ("unary operators " ++ String.join ", " (Set.toList <| Set.map String.fromChar opChars)) <|
            Parser.oneOf
                [ Parser.succeed Expression.UnaryOperator
                    |= spaceIgnoringParser (operatorSymbol opChars)
                    |= spaceIgnoringParser nextParser
                , spaceIgnoringParser nextParser
                ]


spaceIgnoringParser : Parser a -> Parser a
spaceIgnoringParser parser =
    Parser.succeed identity
        |. spaces
        |= parser
        |. spaces


operatorSymbol : Set Char -> Parser Char
operatorSymbol opSymbols =
    Parser.oneOf
        (List.map
            (\c ->
                Parser.symbol (String.fromChar c)
                    |> Parser.map (always c)
            )
            (Set.toList opSymbols)
        )


symbolParser : Parser Expression
symbolParser =
    Parser.keep Parser.oneOrMore isValidSymbolChar
        |> Parser.andCatch (\problem -> Parser.fail "Required a valid symbol character")
        |> Parser.map Expression.Symbol
        |> Parser.inContext "symbol"


spaces : Parser ()
spaces =
    Parser.ignore Parser.zeroOrMore (\c -> c == ' ')


charInRange lower upper =
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
    in
        Parser.keep Parser.oneOrMore (testInRange 'a' 'z')



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
