port module Main exposing (main)

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


initialModel : String -> (Model, Cmd never)
initialModel input =
    ( { input = input
      , scope = Dict.empty
      }
    , Cmd.none
    )


type Msg
    = NewContent String
    | ScopeChanged String String


main : Program String Model Msg
main =
    Browser.document
        { init = initialModel
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }

port saveInput : String -> Cmd never


-- UPDATE


update : Msg -> Model -> (Model, Cmd never)
update msg model =
    case msg of
        NewContent content ->
            ( { model | input = content }
            , saveInput content
            )

        ScopeChanged symbol value ->
            ({ model
                | scope =
                    model.scope |> Dict.insert symbol value
            }
            , Cmd.none
            )



-- VIEW


view : Model -> Browser.Document Msg
view { input, scope } =
    let
        parsed =
            MathParser.expression input

        prompt =
            "> "
    in
    { title = "Parse Maths"
    , body = [ Element.layout
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
                                (\symbol ->
                                    Dict.get symbol scope
                                )
                                res

                        Err err ->
                            [ ErrorDialog.view err ]
                   )
    ]
    }


type ExpressionEvaluationError
    = CannotConvertFloatToInt String
    | UnrecognisedFunctionName String


displayParserResult : (String -> Maybe String) -> MathParser.ParserResult -> List (Element Msg)
displayParserResult scope res =
    let
        input =
            MathToString.stringifyExpression res.expression

        symbolToFloat s =
            s
                |> scope
                |> Maybe.withDefault s
                |> String.toFloat
                |> Result.fromMaybe (CannotConvertFloatToInt s)

        createFunction name =
            name
                |> MathFunction.fromString
                |> Maybe.map MathFunction.toRealFunction
                |> Result.fromMaybe (UnrecognisedFunctionName name)

        output =
            res.expression
                |> MathExpression.updateFunctions createFunction
                |> Result.andThen (MathExpression.updateSymbols symbolToFloat)
                |> Result.map MathEvaluator.evaluate
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
