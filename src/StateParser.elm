module StateParser exposing (StateParser, expression)

import Char
import Dict exposing (Dict)
import MaDebug
import MathExpression exposing (MathExpression)
import ParserError exposing (ParserError)
import ParserResult exposing (ParserResult)
import ParserState exposing (ParserState)
import Set


type alias StateParser =
    ParserState -> Result ParserError ParserResult


binaryOperatorsDict : List (Dict Char MathExpression.BinaryOperator)
binaryOperatorsDict =
    [ Dict.singleton '+' MathExpression.Add
        |> Dict.insert '-' MathExpression.Subtract
    , Dict.singleton '*' MathExpression.Multiply
        |> Dict.insert '/' MathExpression.Divide
    ]


unaryOperatorsDict : Dict Char MathExpression.UnaryOperator
unaryOperatorsDict =
    Dict.singleton '+' MathExpression.UnaryAdd
        |> Dict.insert '-' MathExpression.UnarySubtract


{-| Parse an expression.
-}
expression : StateParser
expression =
    MaDebug.log "MathExpression"
        >> ParserState.trimState
        >> List.foldr
            (\opChars nextParser -> binaryOperators opChars nextParser)
            (unaryOperators unaryOperatorsDict (parenthesis symbol))
            binaryOperatorsDict
        >> Result.mapError
            (\({ parseStack } as parserError) ->
                { parserError | parseStack = ParserError.MathExpression :: parseStack }
            )


binaryOperators : Dict Char MathExpression.BinaryOperator -> StateParser -> StateParser
binaryOperators opDict nextParser =
    checkEmptyState (binaryOperatorsSkipping 0 opDict nextParser)


unaryOperators : Dict Char MathExpression.UnaryOperator -> StateParser -> StateParser
unaryOperators opCharsDict nextParser =
    checkEmptyState
        (\state ->
            let
                label =
                    opCharsDict
                        |> Dict.keys
                        |> List.map String.fromChar
                        |> String.join ", "
                        |> String.append "UnaryOperators "

                { source, start } =
                    MaDebug.log label state
            in
            case String.uncons source of
                Just ( opChar, rhs ) ->
                    case Dict.get opChar opCharsDict of
                        Just op ->
                            let
                                parsedRhs =
                                    { source = rhs
                                    , start = start + 1
                                    }
                                        |> ParserState.trimState
                                        |> nextParser
                                        |> Result.mapError
                                            (\parserError ->
                                                case parserError.errorType of
                                                    ParserError.EmptyString ->
                                                        { position = start
                                                        , errorType = ParserError.MissingUnaryOperand
                                                        , parseStack = []
                                                        }

                                                    _ ->
                                                        parserError
                                            )
                                        |> Result.mapError
                                            (\({ parseStack } as parserError) ->
                                                { parserError | parseStack = ParserError.UnaryOperator op :: parseStack }
                                            )
                            in
                            Result.map
                                (\parseResult ->
                                    { parseResult | expression = MathExpression.UnaryOperation op parseResult.expression }
                                )
                                parsedRhs

                        Nothing ->
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
                    MaDebug.log "Parenthesis" state
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
                                                (\parserError ->
                                                    case parserError.errorType of
                                                        ParserError.EmptyString ->
                                                            { position = start + 1
                                                            , errorType = ParserError.EmptyParentheses
                                                            , parseStack = []
                                                            }

                                                        _ ->
                                                            parserError
                                                )
                                            |> Result.mapError
                                                (\({ parseStack } as parserError) ->
                                                    { parserError | parseStack = ParserError.Parentheses :: parseStack }
                                                )
                                   )

                        else
                            Err
                                { errorType = ParserError.MissingClosingParenthesis
                                , position = start + String.length source - 1
                                , parseStack = [ ParserError.Parentheses ]
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
                        { expression = MathExpression.Symbol source
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
                , errorType = ParserError.EmptyString
                , parseStack = []
                }

        _ ->
            nextParser state


binaryOperatorsSkipping : Int -> Dict Char MathExpression.BinaryOperator -> StateParser -> StateParser
binaryOperatorsSkipping numToSkip opDict nextParser ({ source, start } as state) =
    let
        opChars =
            Dict.keys opDict

        label =
            opChars
                |> List.map String.fromChar
                |> String.join ", "
                |> String.append ("BinaryOperators (skipping " ++ String.fromInt numToSkip ++ ") ")
    in
    case ParserState.splitStateSkipping numToSkip opDict (MaDebug.log label state) of
        Just ( lhs, op, rhsAndMore ) ->
            let
                parsedLhsResult =
                    lhs
                        |> ParserState.trimState
                        |> nextParser
                        |> Result.mapError
                            (\parserError ->
                                (case parserError.errorType of
                                    ParserError.EmptyString ->
                                        { parserError
                                            | errorType = ParserError.MissingBinaryOperand ParserError.LeftHandSide
                                            , parseStack = []
                                        }

                                    _ ->
                                        parserError
                                )
                                    |> (\({ parseStack } as improvedParserError) ->
                                            { improvedParserError
                                                | parseStack =
                                                    ParserError.BinaryOperator op ParserError.LeftHandSide :: parseStack
                                            }
                                       )
                            )
            in
            case parsedLhsResult of
                Ok parsedLhs ->
                    binaryOpRhsHelper 0 opDict nextParser parsedLhs op (ParserState.trimState rhsAndMore)

                Err parserError ->
                    case parserError.errorType of
                        ParserError.MissingBinaryOperand _ ->
                            if isOperatorAlsoUnary op then
                                binaryOperatorsSkipping (numToSkip + 1) opDict nextParser state

                            else
                                Err parserError

                        _ ->
                            Err parserError

        Nothing ->
            nextParser state


binaryOpRhsHelper :
    Int
    -> Dict Char MathExpression.BinaryOperator
    -> StateParser
    -> ParserResult
    -> MathExpression.BinaryOperator
    -> StateParser
binaryOpRhsHelper numToSkip opDict nextParser lhs op rhsAndMore =
    case ParserState.splitStateSkipping numToSkip opDict rhsAndMore of
        Just ( nextRhs, nextOp, moreRhs ) ->
            let
                parsedRhs =
                    nextRhs
                        |> ParserState.trimState
                        |> nextParser
                        |> Result.mapError
                            (\({ parseStack } as parserError) ->
                                { parserError | parseStack = ParserError.BinaryOperator op ParserError.RightHandSide :: parseStack }
                            )
            in
            case parsedRhs of
                Ok rhs ->
                    binaryOpRhsHelper
                        0
                        opDict
                        nextParser
                        { expression = MathExpression.BinaryOperation lhs.expression op rhs.expression
                        , symbols = lhs.symbols ++ rhs.symbols
                        }
                        nextOp
                        moreRhs

                Err parserError ->
                    case parserError.errorType of
                        ParserError.MissingBinaryOperand ParserError.RightHandSide ->
                            binaryOpRhsHelper
                                (numToSkip + 1)
                                opDict
                                nextParser
                                lhs
                                op
                                rhsAndMore

                        ParserError.EmptyString ->
                            binaryOpRhsHelper
                                (numToSkip + 1)
                                opDict
                                nextParser
                                lhs
                                op
                                rhsAndMore

                        _ ->
                            Err parserError

        Nothing ->
            rhsAndMore
                |> ParserState.trimState
                |> nextParser
                |> Result.mapError
                    (\parserError ->
                        (case parserError.errorType of
                            ParserError.EmptyString ->
                                { parserError
                                    | errorType = ParserError.MissingBinaryOperand ParserError.RightHandSide
                                    , parseStack = []
                                    , position = parserError.position - 1
                                }

                            _ ->
                                parserError
                        )
                            |> (\({ parseStack } as improvedParserError) ->
                                    { improvedParserError | parseStack = ParserError.BinaryOperator op ParserError.RightHandSide :: parseStack }
                               )
                    )
                |> Result.map
                    (\rhs ->
                        { expression = MathExpression.BinaryOperation lhs.expression op rhs.expression
                        , symbols = lhs.symbols ++ rhs.symbols
                        }
                    )


isOperatorAlsoUnary : MathExpression.BinaryOperator -> Bool
isOperatorAlsoUnary op =
    case op of
        MathExpression.Add ->
            True

        MathExpression.Subtract ->
            True

        MathExpression.Multiply ->
            False

        MathExpression.Divide ->
            False


symbolHelper : ParserState -> Maybe ParserError
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
                        , errorType = ParserError.InvalidChar firstChar
                        , parseStack = [ ParserError.Symbol ]
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
