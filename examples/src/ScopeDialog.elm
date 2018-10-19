module ScopeDialog exposing (view)

import Config
import Dict exposing (Dict)
import Element as E
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region


view : (String -> Maybe String) -> List String -> (String -> String -> msg) -> List (E.Element msg)
view scope symbols tagger =
    let
        definitionBoxes =
            symbols
                |> List.filter (\s -> String.toFloat s == Nothing)
                |> List.map (definitionBox scope tagger)
    in
    if List.isEmpty definitionBoxes then
        []

    else
        E.paragraph
            []
            [ E.text "Define Variables" ]
            :: definitionBoxes


definitionBox : (String -> Maybe String) -> (String -> String -> msg) -> String -> E.Element msg
definitionBox scope tagger symbol =
    E.row
        [ Background.color Config.backgroundColors.input ]
    <|
        [ Input.text
            [ Region.heading 1
            , E.alignLeft
            , E.paddingXY Config.outputPaddingX 0
            , Border.width 0
            , Border.rounded 0
            , Font.color Config.textColors.input
            , Input.focusedOnLoad
            ]
            { onChange = tagger symbol
            , text =
                symbol
                    |> scope
                    |> Maybe.withDefault ""
            , placeholder = Just <| Input.placeholder [] (E.text <| "Enter value of " ++ symbol)
            , label =
                Input.labelLeft
                    [ E.paddingXY Config.outputPaddingX 0
                    , Font.color Config.textColors.input
                    ]
                    (E.text <| symbol ++ " =")
            }
        ]
