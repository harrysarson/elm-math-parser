module StateParser exposing (ParserResult, StateParser, expression)

import Char
import Dict exposing (Dict)
import MaDebug
import MathExpression exposing (MathExpression)
import ParserError exposing (ParserError)
import ParserState exposing (ParserState)
import Set


type alias ParserResult s f =
    { expression : MathExpression s f
    , symbols : List ( String, Int )
    }


type alias StateParser s f =
    ParserState -> Result ParserError (ParserResult s f)


binaryOperatorsDict : List (Dict Char MathExpression.BinaryOperator)
binaryOperatorsDict =
    [ Dict.singleton '+' MathExpression.Add
        |> Dict.insert '-' MathExpression.Subtract
    , Dict.singleton '*' MathExpression.Multiply
        |> Dict.insert '/' MathExpression.Divide
    , Dict.singleton '^' MathExpression.Exponentiate
    ]


unaryOperatorsDict : Dict Char MathExpression.UnaryOperator
unaryOperatorsDict =
    Dict.singleton '+' MathExpression.UnaryAdd
        |> Dict.insert '-' MathExpression.UnarySubtract


{-| Parse an expression.
-}
expression : (ParserState -> Maybe s) -> (ParserState -> Maybe f) -> StateParser s f
expression parseSymbol parseFunction state =
    MaDebug.log "MathExpression" state
        |> expressionHelper parseSymbol parseFunction
        |> Result.mapError
            (\({ parseStack } as parserError) ->
                { parserError
                    | parseStack =
                        ( ParserError.MathExpression
                        , state
                        )
                            :: parseStack
                }
            )


expressionHelper : (ParserState -> Maybe s) -> (ParserState -> Maybe f) -> StateParser s f
expressionHelper parseSymbol parseFunction =
    let
        parsers : List (StateParser s f -> StateParser s f)
        parsers =
            List.map binaryOperators binaryOperatorsDict
                ++ [ unaryOperators unaryOperatorsDict
                   , congugateTranspose
                   , parenthesis parseSymbol parseFunction
                   ]
    in
    ParserState.trimState
        >> List.foldr
            identity
            (symbolOrFunction parseSymbol parseFunction)
            parsers


binaryOperators : Dict Char MathExpression.BinaryOperator -> StateParser s f -> StateParser s f
binaryOperators opDict nextParser =
    checkEmptyState (binaryOperatorsSkipping 0 opDict nextParser)


unaryOperators : Dict Char MathExpression.UnaryOperator -> StateParser s f -> StateParser s f
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
                                rhsState =
                                    { source = rhs
                                    , start = start + 1
                                    }
                                        |> ParserState.trimState

                                parsedRhs =
                                    rhsState
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
                                                { parserError
                                                    | parseStack =
                                                        ( ParserError.UnaryOperator op
                                                        , rhsState
                                                        )
                                                            :: parseStack
                                                }
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


congugateTranspose : StateParser s f -> StateParser s f
congugateTranspose nextParser =
    checkEmptyState
        (\state ->
            let
                { source, start } =
                    MaDebug.log "congugateTranspose" state
            in
            case
                source
                    |> String.reverse
                    |> String.uncons
            of
                Just ( apostrophe, lhsReversed ) ->
                    if apostrophe == '\'' then
                        { source = String.reverse lhsReversed
                        , start = start + 1
                        }
                            |> ParserState.trimState
                            |> nextParser
                            |> Result.map
                                (\parseResult ->
                                    { parseResult | expression = MathExpression.ConjugateTranspose parseResult.expression }
                                )
                            |> Result.mapError
                                (\parserError ->
                                    case parserError.errorType of
                                        ParserError.EmptyString ->
                                            { position = start
                                            , errorType = ParserError.MissingConjugateTransposeOperand
                                            , parseStack = []
                                            }

                                        _ ->
                                            parserError
                                )
                            |> Result.mapError
                                (\({ parseStack } as parserError) ->
                                    { parserError
                                        | parseStack =
                                            ( ParserError.ConjugateTranspose
                                            , state
                                            )
                                                :: parseStack
                                    }
                                )

                    else
                        nextParser state

                Nothing ->
                    nextParser state
        )


parenthesis : (ParserState -> Maybe s) -> (ParserState -> Maybe f) -> StateParser s f -> StateParser s f
parenthesis parseSymbol parseFunction nextParser =
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
                                        let
                                            parenContentState =
                                                { source = parenthesisContent
                                                , start = start + 1
                                                }
                                        in
                                        parenContentState
                                            |> expressionHelper parseSymbol parseFunction
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
                                                    { parserError
                                                        | parseStack =
                                                            ( ParserError.Parentheses
                                                            , parenContentState
                                                            )
                                                                :: parseStack
                                                    }
                                                )
                                   )

                        else
                            Err
                                { errorType = ParserError.MissingClosingParenthesis
                                , position = start + String.length source - 1
                                , parseStack =
                                    [ ( ParserError.Parentheses
                                      , { source = rest
                                        , start = start + 1
                                        }
                                      )
                                    ]
                                }

                    else
                        nextParser state

                Nothing ->
                    nextParser state
        )


symbol : (ParserState -> Maybe s) -> StateParser s f
symbol parseSymbol =
    checkEmptyState
        (\({ source, start } as state) ->
            case symbolHelper (MaDebug.log "Symbol" state) of
                Nothing ->
                    case parseSymbol state of
                        Just parsedSymbol ->
                            Ok <|
                                { expression = MathExpression.Symbol parsedSymbol
                                , symbols = [ ( source, start ) ]
                                }

                        Nothing ->
                            Err
                                { position = start
                                , errorType = ParserError.UndefinedSymbol state.source
                                , parseStack =
                                    [ ( ParserError.Function
                                      , state
                                      )
                                    ]
                                }

                Just error ->
                    Err error
        )


symbolOrFunction : (ParserState -> Maybe s) -> (ParserState -> Maybe f) -> StateParser s f
symbolOrFunction parseSymbol parseFunction =
    checkEmptyState
        (\({ source, start } as state) ->
            case String.split "[" source of
                funcName :: rest0 :: rest ->
                    let
                        bodyAndClosing =
                            String.join "]" (rest0 :: rest)
                    in
                    if String.endsWith "]" bodyAndClosing then
                        bodyAndClosing
                            |> String.dropRight 1
                            |> (\parenthesisContent ->
                                    let
                                        parenContentState =
                                            { source = parenthesisContent
                                            , start = start + String.length funcName + 1
                                            }
                                    in
                                    parenContentState
                                        |> expressionHelper parseSymbol parseFunction
                                        |> Result.mapError
                                            (\parserError ->
                                                case parserError.errorType of
                                                    ParserError.EmptyString ->
                                                        { position = parenContentState.start
                                                        , errorType = ParserError.EmptyParentheses
                                                        , parseStack = []
                                                        }

                                                    _ ->
                                                        parserError
                                            )
                                        |> Result.mapError
                                            (\({ parseStack } as parserError) ->
                                                { parserError
                                                    | parseStack =
                                                        ( ParserError.Function
                                                        , parenContentState
                                                        )
                                                            :: parseStack
                                                }
                                            )
                                        |> Result.andThen
                                            (\parseResult ->
                                                { source = funcName
                                                , start = start
                                                }
                                                    |> parseFunction
                                                    |> Maybe.map
                                                        (\func ->
                                                            { parseResult
                                                                | expression = MathExpression.Function func parseResult.expression
                                                            }
                                                        )
                                                    |> Result.fromMaybe
                                                        { position = start
                                                        , errorType = ParserError.UndefinedFunction funcName
                                                        , parseStack =
                                                            [ ( ParserError.Function
                                                              , { source = funcName
                                                                , start = start
                                                                }
                                                              )
                                                            ]
                                                        }
                                            )
                               )

                    else
                        Err
                            { errorType = ParserError.MissingClosingParenthesis
                            , position = start + String.length source - 1
                            , parseStack =
                                [ ( ParserError.Function
                                  , { source = bodyAndClosing
                                    , start = start + 1
                                    }
                                  )
                                ]
                            }

                _ ->
                    symbol parseSymbol state
        )



-- Helper Functions --


checkEmptyState : StateParser s f -> StateParser s f
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


binaryOperatorsSkipping : Int -> Dict Char MathExpression.BinaryOperator -> StateParser s f -> StateParser s f
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
                                                    ( ParserError.BinaryOperator op ParserError.LeftHandSide
                                                    , lhs
                                                    )
                                                        :: parseStack
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
    -> StateParser s f
    -> ParserResult s f
    -> MathExpression.BinaryOperator
    -> StateParser s f
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
                                { parserError
                                    | parseStack =
                                        ( ParserError.BinaryOperator op ParserError.RightHandSide
                                        , nextRhs
                                        )
                                            :: parseStack
                                }
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
                                    { improvedParserError
                                        | parseStack =
                                            ( ParserError.BinaryOperator op ParserError.RightHandSide
                                            , rhsAndMore
                                            )
                                                :: parseStack
                                    }
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

        MathExpression.Exponentiate ->
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
                        , parseStack =
                            [ ( ParserError.Symbol
                              , { source = String.fromChar firstChar
                                , start = start
                                }
                              )
                            ]
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
