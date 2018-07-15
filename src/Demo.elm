module Demo exposing (main)

import Html exposing (Html, Attribute, beginnerProgram, text, div, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Task
import Parser exposing (run)
import MathematicsParser exposing (expression)


initialModel =
    "4 + 8"


main =
    beginnerProgram
        { model = initialModel
        , view = view
        , update = update
        }



-- UPDATE


type Msg
    = NewContent String


update (NewContent content) oldContent =
    content



-- VIEW


view content =
    div []
        [ input [ defaultValue initialModel, onInput NewContent, myStyle ] []
        , div [ myStyle ] [ text (toString <| run expression content) ]
        ]


myStyle =
    style
        [ ( "width", "100%" )
        , ( "height", "40px" )
        , ( "padding", "10px 0" )
        , ( "font-size", "2em" )
        , ( "text-align", "center" )
        ]
