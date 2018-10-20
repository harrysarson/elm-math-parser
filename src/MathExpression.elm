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
type MathExpression s f
    = BinaryOperation (MathExpression s f) BinaryOperator (MathExpression s f)
    | UnaryOperation UnaryOperator (MathExpression s f)
    | ConjugateTranspose (MathExpression s f)
    | Parentheses (MathExpression s f)
    | Symbol s
    | Function f (MathExpression s f)


{-| An operator with a left and right hand side.
-}
type BinaryOperator
    = Add
    | Subtract
    | Multiply
    | Divide
    | Exponentiate


{-| An operator with only a right hand side.
-}
type UnaryOperator
    = UnaryAdd
    | UnarySubtract
