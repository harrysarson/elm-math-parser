module MathExpression exposing
    ( MathExpression(..)
    , BinaryOperator(..), UnaryOperator(..)
    , mapFunctions, mapSymbols, updateFunctions, updateSymbols
    )

{-| This library contains types needed to represent simple mathematical expressions.


# Types

@docs MathExpression


# Definitions

@docs BinaryOperator, UnaryOperator


# Mapping

These functions allow mapping to different types and values.

@docs MapSymbols, MapFunctions


# Updating

These functions allow the mapping to fail.

@docs UpdateSymbols, UpdateFunctions

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


{-| Create a new expression by applying the given function to all symbols
in the given expression.
-}
mapSymbols : (s1 -> s2) -> MathExpression s1 f -> MathExpression s2 f
mapSymbols func expr =
    let
        mapper =
            mapSymbols func
    in
    case expr of
        BinaryOperation lhs op rhs ->
            BinaryOperation (mapper lhs) op (mapper rhs)

        UnaryOperation op arg ->
            UnaryOperation op (mapper arg)

        ConjugateTranspose arg ->
            ConjugateTranspose (mapper arg)

        Parentheses arg ->
            Parentheses (mapper arg)

        Symbol sym ->
            Symbol (func sym)

        Function name argument ->
            Function name (mapper argument)


{-| Create a new expression by applying the given function to all symbols
in the given expression.
-}
mapFunctions : (f1 -> f2) -> MathExpression s f1 -> MathExpression s f2
mapFunctions func expr =
    let
        mapper =
            mapFunctions func
    in
    case expr of
        BinaryOperation lhs op rhs ->
            BinaryOperation (mapper lhs) op (mapper rhs)

        UnaryOperation op arg ->
            UnaryOperation op (mapper arg)

        ConjugateTranspose arg ->
            ConjugateTranspose (mapper arg)

        Parentheses arg ->
            Parentheses (mapper arg)

        Symbol sym ->
            Symbol sym

        Function name argument ->
            Function (func name) (mapper argument)


{-| Create a new expression by applying the given function to all symbols
in the given expression.

If a function returns an `Err` then that will be returned by `updateSymbols`.

-}
updateSymbols : (s1 -> Result e s2) -> MathExpression s1 f -> Result e (MathExpression s2 f)
updateSymbols func expr =
    let
        updater =
            updateSymbols func
    in
    case expr of
        BinaryOperation lhs op rhs ->
            Result.map2
                (\uLhs uRhs -> BinaryOperation uLhs op uRhs)
                (updater lhs)
                (updater rhs)

        UnaryOperation op arg ->
            Result.map
                (\uArg -> UnaryOperation op uArg)
                (updater arg)

        ConjugateTranspose arg ->
            Result.map
                (\uArg -> ConjugateTranspose uArg)
                (updater arg)

        Parentheses arg ->
            Result.map
                (\uArg -> Parentheses uArg)
                (updater arg)

        Symbol sym ->
            Result.map
                (\uSym -> Symbol uSym)
                (func sym)

        Function name arg ->
            Result.map
                (\uArg -> Function name uArg)
                (updater arg)


{-| Create a new expression by applying the given function to all functions
in the given expression.

If a function returns an `Err` then that will be returned by `updateFunctions`.

-}
updateFunctions : (f1 -> Result e f2) -> MathExpression s f1 -> Result e (MathExpression s f2)
updateFunctions func expr =
    let
        updater =
            updateFunctions func
    in
    case expr of
        BinaryOperation lhs op rhs ->
            Result.map2
                (\uLhs uRhs -> BinaryOperation uLhs op uRhs)
                (updater lhs)
                (updater rhs)

        UnaryOperation op arg ->
            Result.map
                (\uArg -> UnaryOperation op uArg)
                (updater arg)

        ConjugateTranspose arg ->
            Result.map
                (\uArg -> ConjugateTranspose uArg)
                (updater arg)

        Parentheses arg ->
            Result.map
                (\uArg -> Parentheses uArg)
                (updater arg)

        Symbol sym ->
            Ok (Symbol sym)

        Function name arg ->
            Result.map2
                (\uName uArg -> Function uName uArg)
                (func name)
                (updater arg)
