module MathExpression exposing
    ( MathExpression(..)
    , BinaryOperator(..), UnaryOperator(..)
    )

{-| This library contains types needed to represent simple mathematical expressions.


# Types

@docs MathExpression


# Definitions

@docs BinaryOperator, UnaryOperator

-}

import Set exposing (Set)


{-| A mathematical expression.
-}
type MathExpression
    = BinaryOperation MathExpression BinaryOperator MathExpression
    | UnaryOperation UnaryOperator MathExpression
    | Parentheses MathExpression
    | Symbol String


{-| An operator with a left and right hand side.
-}
type BinaryOperator
    = Add
    | Subtract
    | Multiply
    | Divide


{-| An operator with only a right hand side.
-}
type UnaryOperator
    = UnaryAdd
    | UnarySubtract
