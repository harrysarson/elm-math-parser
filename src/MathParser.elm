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


type alias ParserResult f =
    StateParser.ParserResult f


{-| A parser that converts a string into a mathematical expression.
-}
type alias MathParser f =
    String -> Result ParserError (ParserResult f)


{-| Parse an expression.
-}
expression : (ParserState -> Maybe f) -> MathParser f
expression parseFunction str =
    { source = str
    , start = 0
    }
        |> StateParser.expression parseFunction



-- Parsers --
