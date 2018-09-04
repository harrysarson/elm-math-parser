module StateParser exposing (StateParser, expression)

import Char
import Expression exposing (Expression)
import MaDebug
import ParseError exposing (ParseError)
import ParseResult exposing (ParseResult)
import ParserState exposing (ParserState)
import Set


type alias StateParser =
    ParserState -> Result ParseError ParseResult


{-| Parse an expression.
-}
expression : StateParser
expression =
    MaDebug.log "Expression"
        >> ParserState.trimState
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
        (\state ->
            let
                { source, start } =
                    MaDebug.log "UnaryOperator" state
            in
            case String.uncons source of
                Just ( op, rhs ) ->
                    if List.any (\c -> c == op) opChars then
                        let
                            parsedRhs =
                                { source = rhs
                                , start = start + 1
                                }
                                    |> ParserState.trimState
                                    |> nextParser
                                    |> Result.mapError
                                        (\({ parseStack } as parseError) ->
                                            { parseError | parseStack = ParseError.UnaryOperator op :: parseStack }
                                        )
                        in
                        Result.map
                            (\parseResult ->
                                { parseResult | expression = Expression.UnaryOperator op parseResult.expression }
                            )
                            parsedRhs

                    else
                        nextParser state

                Nothing ->
                    nextParser state
        )


parenthesis : StateParser -> StateParser
parenthesis nextParser =
    checkEmptyState
        (\state ->
            let
                { source, start } =
                    MaDebug.log "UnaryOperator" state
            in
            case String.uncons source of
                Just ( possiblyOpenParenthesis, rest ) ->
                    if possiblyOpenParenthesis == '(' then
                        if String.endsWith ")" rest then
                            rest
                                |> String.dropRight 1
                                |> (\parenthesisContent ->
                                        { source = parenthesisContent
                                        , start = start + 1
                                        }
                                            |> expression
                                            |> Result.mapError
                                                (\parseError ->
                                                    case parseError.errorType of
                                                        ParseError.EmptyString ->
                                                            { position = start + 1
                                                            , errorType = ParseError.EmptyParentheses
                                                            , parseStack = []
                                                            }

                                                        _ ->
                                                            parseError
                                                )
                                            |> Result.mapError
                                                (\({ parseStack } as parseError) ->
                                                    { parseError | parseStack = ParseError.Parentheses :: parseStack }
                                                )
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
                    Ok <|
                        { expression = Expression.Symbol source
                        , symbols = [ ( source, start ) ]
                        }

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
                |> String.append ("BinaryOperators (skipping " ++ String.fromInt numToSkip ++ ") ")
    in
    case ParserState.splitStateSkipping numToSkip opChars (MaDebug.log label state) of
        Just ( lhs, op, rhsAndMore ) ->
            let
                parsedLhsResult =
                    lhs
                        |> ParserState.trimState
                        |> nextParser
                        |> Result.mapError
                            (\({ parseStack } as parseError) ->
                                { parseError | parseStack = ParseError.BinaryOperator op ParseError.LeftHandSide :: parseStack }
                            )
            in
            case parsedLhsResult of
                Ok parsedLhs ->
                    binaryOpRhsHelper 0 opChars nextParser parsedLhs op (ParserState.trimState rhsAndMore)

                Err parseError ->
                    case parseError.errorType of
                        ParseError.EmptyString ->
                            binaryOperatorsSkipping (numToSkip + 1) opChars nextParser state

                        _ ->
                            Err parseError

        Nothing ->
            nextParser state


binaryOpRhsHelper : Int -> List Char -> StateParser -> ParseResult -> Char -> StateParser
binaryOpRhsHelper numToSkip opChars nextParser lhs op rhsAndMore =
    case ParserState.splitStateSkipping numToSkip opChars rhsAndMore of
        Just ( nextRhs, nextOp, moreRhs ) ->
            let
                parsedRhs =
                    nextRhs
                        |> ParserState.trimState
                        |> nextParser
                        |> Result.mapError
                            (\({ parseStack } as parseError) ->
                                { parseError | parseStack = ParseError.BinaryOperator op ParseError.RightHandSide :: parseStack }
                            )
            in
            case parsedRhs of
                Ok rhs ->
                    binaryOpRhsHelper
                        0
                        opChars
                        nextParser
                        { expression = Expression.BinaryOperator lhs.expression op rhs.expression
                        , symbols = lhs.symbols ++ rhs.symbols
                        }
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
                |> ParserState.trimState
                |> nextParser
                |> Result.mapError
                    (\({ parseStack } as parseError) ->
                        { parseError
                            | parseStack = ParseError.BinaryOperator op ParseError.RightHandSide :: parseStack
                        }
                    )
                |> Result.map
                    (\rhs ->
                        { expression = Expression.BinaryOperator lhs.expression op rhs.expression
                        , symbols = lhs.symbols ++ rhs.symbols
                        }
                    )


symbolHelper : ParserState -> Maybe ParseError
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
