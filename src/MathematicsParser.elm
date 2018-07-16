module MathematicsParser exposing (MathematicsParser, expression)

{-| This library allows simple mathematical expressions to be parsed in elm.
A string expression is converted into an abstract syntax tree.


# Mathematics Parsers

@docs MathematicsParser#
@docs expression

-}

import Expression exposing (Expression, binaryOperators, isValidSymbolChar)
import Parser exposing (Parser, (|.), (|=))


{-| A mathematical expression.
-}
type alias MathematicsParser =
    Parser Expression


toBinaryOp op expression rhsExtraList =
    case rhsExtraList of
        [] ->
            expression

        nextRhs :: futureRhs ->
            toBinaryOp op (Expression.BinaryOperator expression op nextRhs) futureRhs


operator : Char -> Parser Expression
operator opChar =
    let
        lhsParser =
            Parser.succeed identity
                |. spaces
                |= symbolParser
                |. spaces
    in
        Parser.inContext ("binary operator " ++ String.fromChar opChar) <|
            Parser.oneOf
                [ Parser.delayedCommitMap (toBinaryOp opChar) lhsParser <|
                    Parser.succeed identity
                        |= Parser.repeat Parser.oneOrMore
                            (Parser.succeed identity
                                |. Parser.symbol (String.fromChar opChar)
                                |. spaces
                                |= symbolParser
                                |. spaces
                            )
                , symbolParser
                ]


symbolParser : Parser Expression
symbolParser =
    Parser.inContext "symbol" <|
        Parser.succeed Expression.Symbol
            |= Parser.keep Parser.oneOrMore isValidSymbolChar


spaces : Parser ()
spaces =
    Parser.ignore Parser.zeroOrMore (\c -> c == ' ')


{-| Parse expressions.
-}
expression : Parser Expression
expression =
    operator '+'
        |. Parser.end
