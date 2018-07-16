module Demo exposing (main)

import Html exposing (Html, Attribute, beginnerProgram, text, div, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Task
import Parser exposing (run)
import MathematicsParser exposing (expression)
import Expression exposing (stringify)


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
    let
        parsed =
            run expression content
    in
        div [] <|
            [ input [ defaultValue initialModel, onInput NewContent, myStyle ] []
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
