port module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Complex
import Complex.Evaluator
import Complex.Function
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
import Json.Decode as Decode
import Json.Encode as Encode
import MathExpression
import MathParser
import MathToString
import Real.Evaluator
import Real.Function
import ScopeDialog
import Task


type Mode
    = Real
    | Complex


type alias Model =
    { input : String
    , scope : Dict String String
    , mode : Mode
    }


initialModel : Decode.Value -> ( Model, Cmd never )
initialModel flags =
    let
        input =
            Decode.decodeValue (Decode.field "input" Decode.string) flags
                |> Result.withDefault "7 + 2 / sqrt[x]"

        scope =
            Decode.decodeValue (Decode.field "scope" <| Decode.dict Decode.string) flags
                |> Result.withDefault (Dict.singleton "x" "4")

        mode =
            Decode.decodeValue (Decode.field "mode" <| Decode.string) flags
                |> Result.toMaybe
                |> Maybe.andThen
                    (\modeString ->
                        case modeString of
                            "real" ->
                                Just Real

                            "complex" ->
                                Just Complex

                            _ ->
                                Nothing
                    )
                |> Maybe.withDefault Real
    in
    ( { input = input
      , scope = scope
      , mode = mode
      }
    , Cmd.none
    )


type Msg
    = NewContent String
    | ScopeChanged String String
    | ModeChanged Mode


main : Program Decode.Value Model Msg
main =
    Browser.document
        { init = initialModel
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


port save : Encode.Value -> Cmd never



-- UPDATE


update : Msg -> Model -> ( Model, Cmd never )
update msg model =
    let
        newModel =
            case msg of
                NewContent content ->
                    { model | input = content }

                ScopeChanged symbol value ->
                    { model
                        | scope =
                            model.scope |> Dict.insert symbol value
                    }

                ModeChanged mode ->
                    { model | mode = mode }
    in
    ( newModel
    , encodeModel newModel
        |> save
    )



-- VIEW


view : Model -> Browser.Document Msg
view { input, scope, mode } =
    let
        parsed =
            MathParser.expression input

        prompt =
            "> "
    in
    { title = "Parse Maths"
    , body =
        [ Element.layout
            [ Background.color Config.backgroundColors.stage
            , Font.color Config.textColors.output
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
                                    mode
                                    (\symbol ->
                                        Dict.get symbol scope
                                    )
                                    res

                            Err err ->
                                [ ErrorDialog.view err ]
                       )
                    ++ [ Input.radio
                            [ Element.padding 10 ]
                            { label =
                                Input.labelAbove
                                    [ Element.padding 10 ]
                                    (Element.text "Select mode")
                            , onChange = ModeChanged
                            , options =
                                [ Input.option Real <| Element.text "real"
                                , Input.option Complex <| Element.text "complex"
                                ]
                            , selected = Just mode
                            }
                       ]
        ]
    }


type ExpressionEvaluationError
    = CannoInterpretSymbolAsValue String
    | UnrecognisedFunctionName String


displayParserResult : Mode -> (String -> Maybe String) -> MathParser.ParserResult -> List (Element Msg)
displayParserResult mode scope res =
    let
        input =
            MathToString.stringifyExpression res.expression

        symbolToValue parser s =
            s
                |> scope
                |> Maybe.withDefault s
                |> parser
                |> Result.fromMaybe (CannoInterpretSymbolAsValue s)

        createComplexFunction name =
            name
                |> Complex.Function.fromString
                |> Maybe.map Complex.Function.toFunction
                |> Result.fromMaybe (UnrecognisedFunctionName name)

        createRealFunction name =
            name
                |> Real.Function.fromString
                |> Maybe.map Real.Function.toFunction
                |> Result.fromMaybe (UnrecognisedFunctionName name)

        output =
            case mode of
                Real ->
                    res.expression
                        |> MathExpression.updateFunctions createRealFunction
                        |> Result.andThen (MathExpression.updateSymbols (symbolToValue String.toFloat))
                        |> Result.map Real.Evaluator.evaluate
                        |> Debug.toString

                Complex ->
                    res.expression
                        |> MathExpression.updateFunctions createComplexFunction
                        |> Result.andThen (MathExpression.updateSymbols (symbolToValue Complex.fromString))
                        |> Result.map Complex.Evaluator.evaluate
                        |> Result.map Complex.toString
                        |> Debug.toString

        symbols =
            res
                |> .symbols
                |> List.map Tuple.first
                |> List.filter (\s -> Complex.fromString s == Nothing)
    in
    ScopeDialog.view scope symbols ScopeChanged
        ++ [ Element.paragraph
                [ Element.padding 10 ]
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


encodeModel : Model -> Encode.Value
encodeModel model =
    Encode.object
        [ ( "input", Encode.string model.input )
        , ( "scope"
          , model.scope
                |> Encode.dict identity Encode.string
          )
        ]
