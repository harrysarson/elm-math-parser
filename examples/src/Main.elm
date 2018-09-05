module Demo exposing (main)

import Browser
import Browser.Dom as Dom
import Html exposing (Attribute, Html, div, input, li, ol, span, table, td, text, tr)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import MathExpression
import MathParser exposing (expression)
import MathToString
import Task


initialModel : String
initialModel =
    "4 + 8"


main : Program () String Msg
main =
    Browser.document
        { init = \_ -> ( initialModel, Task.attempt (always FocusResult) (Dom.focus "expression-input") )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- UPDATE


type Msg
    = NewContent String
    | FocusResult


update : Msg -> String -> ( String, Cmd msg )
update msg oldContent =
    case msg of
        NewContent content ->
            ( content, Cmd.none )

        FocusResult ->
            ( oldContent, Cmd.none )



-- VIEW


flipResult : Result value a -> Result a value
flipResult result =
    case result of
        Err err ->
            Ok err

        Ok val ->
            Err val


view : String -> Browser.Document Msg
view content =
    let
        parsed =
            expression (always Nothing) content

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
                |> Result.map (\p -> div [] [ text (MathToString.stringifyExpression (\_ -> "ERROR") p.expression) ])
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
