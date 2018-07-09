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


operator : Char -> Parser Expression
operator opChar =
    let
        lhsParser =
            Parser.succeed identity
                |. spaces
                |= symbolParser
                |. spaces

        expresionWithoutLhs =
            (\c rhs -> (\lhs -> Expression.BinaryOperator lhs c rhs))
    in
        Parser.inContext ("binary operator " ++ String.fromChar opChar) <|
            --     Parser.delayedCommitMap (\lhs ( op, rhs ) -> Expression.BinaryOperator lhs op rhs) lhsParser <|
            Parser.succeed Expression.BinaryOperator
                |= symbolParser
                |= (Parser.symbol (String.fromChar opChar)
                        |> Parser.map (always opChar)
                   )
                |. spaces
                |= symbolParser
                |. spaces


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
