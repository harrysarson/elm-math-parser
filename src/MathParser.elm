module MathParser exposing
    ( MathParser
    , expression, standardExpression
    )

{-| This library allows simple mathematical expressions to be parsed in elm.
A string expression is converted into an abstract syntax tree.


# Mathematics Parsers

@docs MathParser
@docs expression, standardExpression

-}

import Char
import MaDebug
import MathFunction exposing (MathFunction)
import ParserError exposing (ParserError)
import ParserResult exposing (ParserResult)
import Set
import StateParser


{-| A parser that converts a string into a mathematical expression.
-}
type alias MathParser f =
    String -> Result ParserError (ParserResult f)


{-| Parse an expression.
-}
expression : (String -> Maybe f) -> MathParser f
expression stringToFunction str =
    { source = str
    , start = 0
    }
        |> StateParser.expression stringToFunction


{-| Parse an expression using standard mathematic functions.
-}
standardExpression : MathParser MathFunction
standardExpression str =
    { source = str
    , start = 0
    }
        |> StateParser.expression MathFunction.fromString



-- Parsers --
