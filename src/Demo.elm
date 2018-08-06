module Demo exposing (main)

import Html exposing (Html, Attribute, program, text, div, input, ol, li, table, tr, td, span)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Dom
import Task
import MathematicsParser exposing (expression)
import Expression exposing (stringify)


initialModel : String
initialModel =
    "4 + 8"


main : Program Never String Msg
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


view : String -> Html Msg
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
                    (always <| span [] [ text "Parser stack" ])
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
                |> Result.map (\p -> div [] [ text (stringify p.expression) ])
                |> Result.toMaybe
             , parsed
                |> Result.map .symbols
                |> Result.map
                    (\symbols ->
                        table
                            [ style [ ( "border-collapse", "collapse" ), ( "text-align", "center" ) ] ]
                            ((tr [ style [ ( "border-bottom", "1px solid black" ) ] ]
                                [ td [] [ text "Symbol" ]
                                , td [] [ text "Index" ]
                                ]
                             )
                                :: (symbols
                                        |> List.map
                                            (\( symbol, index ) ->
                                                tr [] [ td [] [ text symbol ], td [] [ text <| toString index ] ]
                                            )
                                   )
                            )
                    )
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


myStyle : Attribute msg
myStyle =
    style
        [ ( "padding", "1em 1em" )
        , ( "font-size", "2em" )
        , ( "font-family", "monospace, monospace" )
        ]


inputStyle : Attribute msg
inputStyle =
    style
        [ ( "width", "100%" )
        , ( "font", "inherit" )
        ]


tableStyle : Attribute msg
tableStyle =
    style [ ( "border-collapse", "collapse" ) ]
