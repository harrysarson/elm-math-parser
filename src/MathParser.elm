module MathParser exposing
    ( expression
    , MathParser
    )

{-| This library allows simple mathematical expressions to be parsed in elm.
A string expression is converted into an abstract syntax tree.


# Mathematics Parsers

@docs MathParser#
@docs expression

-}

import Char
import MaDebug
import ParserError exposing (ParserError)
import ParserResult exposing (ParserResult)
import Set
import StateParser


{-| A parser that converts a string into a mathematical expression.
-}
type alias MathParser f =
    String -> Result ParserError (ParserResult f)


{-| }
} Parse an expression.
-}
expression : (String -> Maybe f) -> MathParser f
expression stringToFunction str =
    { source = str
    , start = 0
    }
        |> StateParser.expression stringToFunction



-- Parsers --
