module ParserError exposing (ErrorType(..), ParseFunction(..), ParserError, Side(..))

{-| Definition of errors created when passing state


# Definition

@docs ParserError ErrorType ParseFunction Side

-}

import MathExpression
import ParserState exposing (ParserState)


{-| An error created whilst parsing an expression.

  - `position`: position of character in parser input the generated the error.
  - `errorType`: type of error.
  - `parseStack`: list of parser functions in order called, used to generated error messages
    like "error found in the left hand side of the binary '\*' operator: ..."

-}
type alias ParserError =
    { position : Int
    , errorType : ErrorType
    , parseStack : List ( ParseFunction, ParserState )
    }


{-| Type of error. Not all of these are fatal for example `EmptyString` errors
cause the parsing to unwind and try again maybe parsing a unary rather than
binary operator.
-}
type ErrorType
    = EmptyString
    | EmptyParentheses
    | InvalidChar Char
    | MissingBinaryOperand Side
    | MissingUnaryOperand
    | MissingConjugateTransposeOperand
    | MissingClosingParenthesis
    | UndefinedFunction String


{-| Different parser functions used to track the location of a parsing error.
-}
type ParseFunction
    = Symbol
    | BinaryOperator MathExpression.BinaryOperator Side
    | UnaryOperator MathExpression.UnaryOperator
    | ConjugateTranspose
    | Parentheses
    | Function
    | MathExpression


{-| Side of a binary operator being parsed when an error was encountered.
-}
type Side
    = RightHandSide
    | LeftHandSide
