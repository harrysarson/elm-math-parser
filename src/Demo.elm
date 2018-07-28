module Demo exposing (main)

import Html exposing (Html, Attribute, program, text, div, input, ol, li)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Dom
import Task
import Parser exposing (run)
import MathematicsParser exposing (expression)
import Expression exposing (stringify)


initialModel =
    "4 + 8"


main =
    program
        { init = ( initialModel, Task.attempt (always FocusResult) (Dom.focus "expression-input") )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- UPDATE


type Msg
    = NewContent String
    | FocusResult


update msg oldContent =
    case msg of
        NewContent content ->
            ( content, Cmd.none )

        FocusResult ->
            ( oldContent, Cmd.none )



-- VIEW


flipResult result =
    case result of
        Err err ->
            Ok err

        Ok val ->
            Err val


view content =
    let
        parsed =
            expression content

        numbers =
            content
                |> String.toList
                |> List.indexedMap always
                |> List.map (\i -> i % 10)
                |> List.map toString
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
        div [ myStyle ]
            ([ Just <|
                input
                    [ defaultValue initialModel
                    , id "expression-input"
                    , onInput NewContent
                    , inputStyle
                    ]
                    []
             , errPointer
                |> Maybe.map
                    (\pointer ->
                        input
                            [ value pointer
                            , inputStyle
                            ]
                            []
                    )
             , error
                |> Maybe.map .parseStack
                |> Maybe.map
                    (\stack ->
                        ol [] <| List.map (\fun -> li [] [ text <| toString fun ]) stack
                    )
             , error
                |> Maybe.map .errorType
                |> Maybe.map
                    (\type_ ->
                        div [] [ text <| "type: " ++ toString type_ ]
                    )
             , Just <| div [] [ text (toString parsed) ]
             , parsed
                |> Result.map (\p -> div [] [ text (stringify p) ])
                |> Result.toMaybe
             ]
                |> justList
            )


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


myStyle =
    style
        [ ( "padding", "1em 1em" )
        , ( "font-size", "2em" )
        , ( "font-family", "monospace, monospace" )
        ]


inputStyle =
    style
        [ ( "width", "100%" )
        , ( "font", "inherit" )
        ]
