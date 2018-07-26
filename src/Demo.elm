module Demo exposing (main)

import Html exposing (Html, Attribute, program, text, div, input)
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


view content =
    let
        parsed =
            run expression content
    in
        div [] <|
            [ input [ defaultValue initialModel, id "expression-input", onInput NewContent, myStyle ] []
            , div [ myStyle ] [ text (toString parsed) ]
            ]
                ++ (parsed
                        |> Result.map (\p -> [ div [ myStyle ] [ text (stringify p) ] ])
                        |> Result.withDefault []
                   )


myStyle =
    style
        [ ( "width", "100%" )
        , ( "padding", "10px 0" )
        , ( "font-size", "2em" )
        , ( "text-align", "center" )
        ]
