module ErrorDialog exposing (view)

import Config
import Element as E
import Element.Background as B
import Element.Border as Border
import Element.Font as F
import Element.Region as R
import MathToString
import ParserError exposing (ParserError)


view : ParserError -> E.Element msg
view error =
    let
        ( heading, info ) =
            typeToString error

        maybeLastFunc =
            error.parseStack
                |> List.reverse
                |> List.head
    in
    E.column
        [ B.color Config.backgroundColors.error
        , E.padding 8
        , E.spacing 8
        , F.color Config.textColors.error
        , F.size Config.textSize
        , F.family [ F.typeface "Helvetica" ]
        ]
    <|
        [ E.paragraph
            [ E.centerX
            , E.width E.shrink
            , R.heading 2
            , F.size (Config.textSize * 3 // 2)
            ]
            ((case maybeLastFunc of
                Just ( lastFunc, state ) ->
                    [ E.text "Error whilst passing"
                    , mathSnippet state.source
                    , E.text ":"
                    ]

                Nothing ->
                    []
             )
                ++ [ E.text heading ]
            )
        , E.paragraph
            []
            info
        , E.table
            [ F.family [ F.typeface "monospace" ]
            ]
            { data = error.parseStack
            , columns =
                [ { header = E.el [ F.bold ] (E.text "Function")
                  , width = E.fillPortion 1
                  , view =
                        Tuple.first
                            >> Debug.toString
                            >> E.text
                  }
                , { header = E.el [ F.bold ] (E.text "Source")
                  , width = E.fillPortion 5
                  , view =
                        Tuple.second
                            >> (\state ->
                                    let
                                        nonBreakingSpace =
                                            "\u{00A0}"
                                    in
                                    String.repeat state.start nonBreakingSpace
                                        ++ "\""
                                        ++ state.source
                                        ++ "\""
                               )
                            >> E.text
                  }
                , { header = E.el [ F.bold ] (E.text "Position")
                  , width = E.fillPortion 5
                  , view =
                        Tuple.second
                            >> .start
                            >> String.fromInt
                            >> E.text
                  }
                ]
            }
        , E.text ""
        , E.paragraph
            []
            [ E.text <| Debug.toString error ]
        ]


mathSnippet : String -> E.Element msg
mathSnippet text =
    E.el
        [ B.color (E.rgba 1 1 1 1)
        , Border.rounded 1
        , E.paddingXY 5 0
        , F.family [ F.monospace ]
        ]
        (E.text text)


typeToString : ParserError -> ( String, List (E.Element msg) )
typeToString error =
    case error.errorType of
        ParserError.EmptyString ->
            ( "Empty String", [] )

        ParserError.EmptyParentheses ->
            ( "Empty Parentheses", [] )

        ParserError.InvalidChar c ->
            ( "Invalid Character"
            , [ E.text <| "The character '" ++ String.fromChar c ++ "' is not allowed in symbols."
              ]
            )

        ParserError.MissingBinaryOperand s ->
            let
                maybeLastFunc =
                    error.parseStack
                        |> List.reverse
                        |> List.head
            in
            ( "Missing Operand in Binary Expression"
            , [ E.text "The"
              , E.text
                    (case s of
                        ParserError.LeftHandSide ->
                            "left"

                        ParserError.RightHandSide ->
                            "right"
                    )
              , E.text "hand side of the operator"
              , E.text <|
                    case maybeLastFunc of
                        Just ( func, _ ) ->
                            case func of
                                ParserError.BinaryOperator op _ ->
                                    MathToString.stringifyBinaryOperator op

                                _ ->
                                    ""

                        Nothing ->
                            ""
              , E.text "is missing."
              ]
            )

        -- ParserError.ExponentialWithUnaryOperatorLhs ->
        --     ( "The exponential operator's left hand side cannot be a unary function"
        --     , [ E.text "It is confusing if the left hand side of an exponential is a unary operator as it is not clear what the precidence should be."
        --       , E.text "Add brackets to clarify your intention."
        --       , E.text "Instead of"
        --       , mathSnippet "-2^x"
        --       , E.text " use either"
        --       , mathSnippet "(-2)^x"
        --       , E.text " or"
        --       , mathSnippet "-(2^x)"
        --       , E.text "."
        --       ]
        --     )
        other ->
            ( Debug.toString other, [] )
