module MathParser exposing
    ( ParserResult, MathParser
    , expression
    )

{-| This library allows simple mathematical expressions to be parsed in elm.
A string expression is converted into an abstract syntax tree.


# Mathematics Parsers

@docs ParserResult, MathParser
@docs expression

-}

import ParserError exposing (ParserError)
import ParserState exposing (ParserState)
import StateParser


type alias ParserResult =
    StateParser.ParserResult


{-| A parser that converts a string into a mathematical expression.
-}
type alias MathParser =
    String -> Result ParserError ParserResult


{-| Parse an expression.
-}
expression : MathParser
expression str =
    { source = str
    , start = 0
    }
        |> StateParser.expression



-- Parsers --
