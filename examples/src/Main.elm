module Demo exposing (main)

import Browser
import Browser.Dom as Dom
import Config
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import ErrorDialog
import Html exposing (Attribute, Html, div, input, li, ol, span, table, td, text, tr)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import MathEvaluator
import MathExpression
import MathFunction exposing (MathFunction)
import MathParser
import MathToString
import ScopeDialog
import Task


type alias Model =
    { input : String
    , scope : Dict String String
    }


initialModel : Model
initialModel =
    { input = "4 + 8 *"
    , scope = Dict.empty
    }


type Msg
    = NewContent String
    | ScopeChanged String String


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        NewContent content ->
            { model | input = content }

        ScopeChanged symbol value ->
            { model
                | scope =
                    model.scope |> Dict.insert symbol value
            }



-- VIEW


view : Model -> Html Msg
view { input, scope } =
    let
        parsed =
            MathParser.standardExpression input

        prompt =
            "> "
    in
    Element.layout
        [ Background.color Config.backgroundColors.stage
        , Font.color Config.textColors.output
        , Font.italic
        , Font.size (Config.textSize * 2)
        , Font.family [ Font.monospace ]
        ]
    <|
        Element.column
            [ Element.width Element.fill
            , Element.height Element.shrink
            , Element.centerX
            , Element.spacing 36
            , Element.paddingXY 0 36
            ]
        <|
            [ Element.el
                [ Region.heading 1
                , Element.paddingXY Config.outputPaddingX 0
                ]
              <|
                Element.text "Enter a mathematical expression"
            , Element.column
                [ Element.width Element.fill
                , Font.color Config.textColors.input
                ]
              <|
                Input.text
                    [ Element.alignLeft
                    , Element.paddingXY Config.outputPaddingX 0
                    , Element.spacing 0
                    , Element.focused
                        [ Border.glow (Element.rgba 0 1 0 1) 0 ]
                    , Border.width 0
                    , Border.rounded 0
                    , Input.focusedOnLoad
                    ]
                    { onChange = NewContent
                    , text = input
                    , placeholder = Nothing
                    , label =
                        Input.labelLeft
                            [ Background.color Config.backgroundColors.input ]
                            (Element.text prompt)
                    }
                    :: (case parsed of
                            Ok _ ->
                                []

                            Err err ->
                                let
                                    nonBreakingSpace =
                                        "\u{00A0}"

                                    promptAsSpaces =
                                        String.repeat (String.length prompt) nonBreakingSpace

                                    pointerText =
                                        err.position
                                            |> (\p -> String.repeat p nonBreakingSpace)
                                            |> String.cons '^'
                                            |> String.reverse
                                            |> String.padRight (String.length input) ' '
                                in
                                [ Element.el
                                    []
                                    (Element.text promptAsSpaces)
                                , Element.el
                                    [ Element.width Element.fill
                                    , Element.paddingXY Config.outputPaddingX 0
                                    ]
                                    (Element.text pointerText)
                                ]
                                    |> Element.row
                                        [ Element.alignLeft
                                        , Element.paddingXY Config.outputPaddingX 0
                                        , Element.width Element.fill
                                        , Font.color Config.backgroundColors.input
                                        , Element.padding 0
                                        ]
                                    |> List.singleton
                       )
            ]
                ++ (case parsed of
                        Ok res ->
                            displayParserResult
                                (\symbol ->
                                    Dict.get symbol scope
                                )
                                res

                        Err err ->
                            [ ErrorDialog.view err ]
                   )


displayParserResult : (String -> Maybe String) -> MathParser.ParserResult MathFunction -> List (Element Msg)
displayParserResult scope res =
    let
        input =
            MathToString.stringifyExpression MathFunction.toString res.expression

        floatScope =
            scope
                >> Maybe.andThen String.toFloat

        output =
            MathEvaluator.evaluateWithScope MathFunction.toRealFunction floatScope res.expression
                |> Debug.toString

        symbols =
            res
                |> .symbols
                |> List.map Tuple.first
    in
    ScopeDialog.view scope symbols ScopeChanged
        ++ [ Element.paragraph
                []
                [ Element.text "Result" ]
           , Element.el
                [ Background.color (Element.rgba 1 1 1 1)
                , Element.width Element.fill
                , Element.paddingXY Config.outputPaddingX 0
                ]
             <|
                Element.paragraph
                    [ Font.color Config.textColors.input ]
                    [ Element.text <| input ++ " = " ++ output ]
           ]


tmp content =
    let
        parsed =
            MathParser.expression (always Nothing) content

        numbers =
            content
                |> String.toList
                |> List.indexedMap always
                |> List.map (\i -> modBy 10 i)
                |> List.map String.fromInt
                |> String.join ""

        error =
            parsed
                |> flipResult
                |> Result.toMaybe

        errorPosition =
            error
                |> Maybe.map .position

        errPointer =
            errorPosition
                |> Maybe.map
                    (\p ->
                        String.repeat p " "
                            |> String.cons '^'
                            |> String.reverse
                            |> String.padRight (String.length content) ' '
                    )
    in
    { title = "Elm Mathematics Parsing"
    , body =
        [ div myStyle
            ([ Just <|
                input
                    ([ value content
                     , id "expression-input"
                     , onInput NewContent
                     ]
                        ++ inputStyle
                    )
                    []
             , errPointer
                |> Maybe.map
                    (\pointer ->
                        input
                            ([ value pointer ]
                                ++ inputStyle
                            )
                            []
                    )
             , error
                |> Maybe.map .parseStack
                |> Maybe.map
                    (always <| span [] [ text "Parser stack" ])
             , error
                |> Maybe.map .parseStack
                |> Maybe.map
                    (\stack ->
                        ol [] <| List.map (\fun -> li [] [ text <| Debug.toString fun ]) stack
                    )
             , error
                |> Maybe.map .errorType
                |> Maybe.map
                    (\type_ ->
                        div [] [ text <| "type: " ++ Debug.toString type_ ]
                    )
             , Just <| div [] [ text (Debug.toString parsed) ]
             , parsed
                |> Result.map (\p -> div [] [ text (MathToString.stringifyExpression MathFunction.toString p.expression) ])
                |> Result.toMaybe
             , parsed
                |> Result.map .symbols
                |> Result.map
                    (\symbols ->
                        table
                            [ style "border-collapse" "collapse", style "text-align" "center" ]
                            (tr [ style "border-bottom" "1px solid black" ]
                                [ td [] [ text "Symbol" ]
                                , td [] [ text "Index" ]
                                ]
                                :: (symbols
                                        |> List.map
                                            (\( symbol, index ) ->
                                                tr [] [ td [] [ text symbol ], td [] [ text <| Debug.toString index ] ]
                                            )
                                   )
                            )
                    )
                |> Result.toMaybe
             , parsed
                |> Result.toMaybe
                |> Maybe.map .expression
                |> Maybe.map (MathEvaluator.evaluate MathFunction.toRealFunction)
                |> Maybe.map Debug.toString
                |> Maybe.map
                    (\s ->
                        div [] [ text s ]
                    )
             ]
                |> justList
            )
        ]
    }


justList : List (Maybe a) -> List a
justList list =
    case list of
        [] ->
            []

        maybeFirst :: rest ->
            case maybeFirst of
                Just first ->
                    first :: justList rest

                Nothing ->
                    justList rest


myStyle : List (Attribute msg)
myStyle =
    [ style "padding" "1em 1em"
    , style "font-size" "2em"
    , style "font-family" "monospace, monospace"
    ]


inputStyle : List (Attribute msg)
inputStyle =
    [ style "width" "100%"
    , style "font" "inherit"
    ]


tableStyle : List (Attribute msg)
tableStyle =
    [ style "border-collapse" "collapse" ]


flipResult : Result value a -> Result a value
flipResult result =
    case result of
        Err err ->
            Ok err

        Ok val ->
            Err val
