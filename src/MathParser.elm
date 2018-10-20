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

import Char
import MaDebug
import MathFunction exposing (MathFunction)
import ParserError exposing (ParserError)
import ParserState exposing (ParserState)
import Set
import StateParser


type alias ParserResult s f =
    StateParser.ParserResult s f


{-| A parser that converts a string into a mathematical expression.
-}
type alias MathParser s f =
    String -> Result ParserError (ParserResult s f)


{-| Parse an expression.
-}
expression : (ParserState -> Maybe s) -> (ParserState -> Maybe f) -> MathParser s f
expression parseString parseFunction str =
    { source = str
    , start = 0
    }
        |> StateParser.expression parseString parseFunction



-- Parsers --
