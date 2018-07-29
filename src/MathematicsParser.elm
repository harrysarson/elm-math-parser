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
import ParseState exposing (ParseState)
import MaDebug


{-| A parser that converts a string into a mathematical expression.
-}
type alias MathematicsParser =
    String -> Result ParseError Expression


type alias StateParser =
    ParseState -> Result ParseError Expression


{-| }
} Parse an expression.
-}
expression : MathematicsParser
expression str =
    { source = str
    , start = 0
    }
        |> stateExpression



-- Parsers --


{-| Parse an expression.
-}
stateExpression : StateParser
stateExpression =
    MaDebug.log "Expression"
        >> ParseState.trimState
        >> List.foldr
            (\opChars nextParser -> binaryOperators opChars nextParser)
            (unaryOperators (Expression.unaryOperators |> Set.toList) (parenthesis symbol))
            (Expression.binaryOperators |> List.map Set.toList)
        >> Result.mapError
            (\({ parseStack } as parseError) ->
                { parseError | parseStack = ParseError.Expression :: parseStack }
            )


binaryOperators : List Char -> StateParser -> StateParser
binaryOperators opChars nextParser =
    checkEmptyState (binaryOperatorsSkipping 0 opChars nextParser)


unaryOperators : List Char -> StateParser -> StateParser
unaryOperators opChars nextParser =
    checkEmptyState
        (\({ source, start } as state) ->
            case String.uncons (MaDebug.log "UnaryOperator" source) of
                Just ( op, rhs ) ->
                    if List.any (\c -> c == op) opChars then
                        let
                            parsedRhs =
                                { source = rhs
                                , start = start + 1
                                }
                                    |> ParseState.trimState
                                    |> nextParser
                                    |> Result.mapError
                                        (\({ parseStack } as parseError) ->
                                            { parseError | parseStack = ParseError.UnaryOperator op :: parseStack }
                                        )
                        in
                            Result.map
                                (Expression.UnaryOperator op)
                                parsedRhs
                    else
                        nextParser state

                Nothing ->
                    nextParser state
        )


parenthesis : StateParser -> StateParser
parenthesis nextParser =
    checkEmptyState
        (\({ source, start } as state) ->
            case String.uncons (MaDebug.log "Parentheses" source) of
                Just ( possiblyOpenParenthesis, rest ) ->
                    if possiblyOpenParenthesis == '(' then
                        if String.endsWith ")" rest then
                            rest
                                |> String.dropRight 1
                                |> \parenthesisContent ->
                                    { source = parenthesisContent
                                    , start = start + 1
                                    }
                                        |> stateExpression
                                        |> Result.mapError
                                            (\({ parseStack } as parseError) ->
                                                { parseError | parseStack = ParseError.Parentheses :: parseStack }
                                            )
                        else
                            Err
                                { errorType = ParseError.MissingClosingParenthesis
                                , position = start + String.length source - 1
                                , parseStack = [ ParseError.Parentheses ]
                                }
                    else
                        nextParser state

                Nothing ->
                    nextParser state
        )


symbol : StateParser
symbol =
    checkEmptyState
        (\({ source, start } as state) ->
            case symbolHelper (MaDebug.log "Symbol" state) of
                Nothing ->
                    Ok <| Expression.Symbol source

                Just error ->
                    Err error
        )



-- Helper Functions --


checkEmptyState : StateParser -> StateParser
checkEmptyState nextParser ({ source, start } as state) =
    case source of
        "" ->
            Err
                { position = start
                , errorType = ParseError.EmptyString
                , parseStack = []
                }

        _ ->
            nextParser state


binaryOperatorsSkipping : Int -> List Char -> StateParser -> StateParser
binaryOperatorsSkipping numToSkip opChars nextParser ({ source, start } as state) =
    let
        label =
            opChars
                |> List.map String.fromChar
                |> String.join ", "
                |> String.append "BinaryOperators "
    in
        case ParseState.splitStateSkipping numToSkip opChars (MaDebug.log label state) of
            Just ( lhs, op, rhsAndMore ) ->
                let
                    parsedLhsResult =
                        lhs
                            |> ParseState.trimState
                            |> nextParser
                            |> Result.mapError
                                (\({ parseStack } as parseError) ->
                                    { parseError | parseStack = (ParseError.BinaryOperator op ParseError.LeftHandSide) :: parseStack }
                                )
                in
                    case parsedLhsResult of
                        Ok parsedLhs ->
                            binaryOpRhsHelper 0 opChars nextParser parsedLhs op (ParseState.trimState rhsAndMore)

                        Err parseError ->
                            case parseError.errorType of
                                ParseError.EmptyString ->
                                    binaryOperatorsSkipping (numToSkip + 1) opChars nextParser state

                                _ ->
                                    Err parseError

            Nothing ->
                nextParser state


binaryOpRhsHelper : Int -> List Char -> StateParser -> Expression -> Char -> ParseState -> Result ParseError Expression
binaryOpRhsHelper numToSkip opChars nextParser lhs op rhsAndMore =
    case ParseState.splitStateSkipping numToSkip opChars rhsAndMore of
        Just ( nextRhs, nextOp, moreRhs ) ->
            let
                parsedRhs =
                    nextRhs
                        |> ParseState.trimState
                        |> nextParser
                        |> Result.mapError
                            (\({ parseStack } as parseError) ->
                                { parseError | parseStack = (ParseError.BinaryOperator op ParseError.RightHandSide) :: parseStack }
                            )
            in
                case parsedRhs of
                    Ok rhs ->
                        binaryOpRhsHelper
                            0
                            opChars
                            nextParser
                            (Expression.BinaryOperator lhs op rhs)
                            nextOp
                            moreRhs

                    Err parseError ->
                        case parseError.errorType of
                            ParseError.EmptyString ->
                                binaryOpRhsHelper (numToSkip + 1) opChars nextParser lhs op rhsAndMore

                            _ ->
                                Err parseError

        Nothing ->
            rhsAndMore
                |> ParseState.trimState
                |> nextParser
                |> Result.mapError
                    (\({ parseStack } as parseError) ->
                        { parseError | parseStack = (ParseError.BinaryOperator op ParseError.RightHandSide) :: parseStack }
                    )
                |> Result.map (Expression.BinaryOperator lhs op)


symbolHelper : ParseState -> Maybe ParseError
symbolHelper ({ source, start } as state) =
    String.uncons source
        |> Maybe.andThen
            (\( firstChar, rest ) ->
                if isValidSymbolChar firstChar then
                    symbolHelper
                        { state
                            | source = rest
                            , start = start + 1
                        }
                else
                    Just
                        { position = start
                        , errorType = ParseError.InvalidChar firstChar
                        , parseStack = [ ParseError.Symbol ]
                        }
            )


isValidSymbolChar : Char -> Bool
isValidSymbolChar charToTest =
    let
        isNumber =
            isCharInRange '0' '9'

        isLowerEnglish =
            isCharInRange 'a' 'z'

        isUpperEnglish =
            isCharInRange 'A' 'Z'

        isPeriod =
            (==) '.'
    in
        isNumber charToTest
            || isLowerEnglish charToTest
            || isUpperEnglish charToTest
            || isPeriod charToTest


isCharInRange : Char -> Char -> Char -> Bool
isCharInRange lower upper char =
    let
        lowerNum =
            Char.toCode lower

        upperNum =
            Char.toCode upper

        charNum =
            Char.toCode char
    in
        charNum >= lowerNum && charNum <= upperNum
