module MathematicsParser exposing (MathematicsParser, expression)

{-| This library allows simple mathematical expressions to be parsed in elm.
A string expression is converted into an abstract syntax tree.


# Mathematics Parsers

@docs MathematicsParser#
@docs expression

-}

import Char
import Set
import Expression exposing (Expression)
import ParseError exposing (ParseError)
import StateParser
import MaDebug


{-| A parser that converts a string into a mathematical expression.
-}
type alias MathematicsParser =
    String -> Result ParseError Expression


{-| }
} Parse an expression.
-}
expression : MathematicsParser
expression str =
    { source = str
    , start = 0
    }
        |> StateParser.expression



-- Parsers --
