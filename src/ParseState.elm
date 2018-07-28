module ParseState exposing (ParseState, splitStateSkipping, trimState)

{-| Definition of state used for parsing and functions to manipulate this state.


# Definition

@docs ParseState


# Functions

@docs splitStateSkipping trimState

-}


{-| The state of a partially parsed expression.

  - `source`: the string to be parsed.
  - `start`: position of source in the original string input.

-}
type alias ParseState =
    { source : String
    , start : Int
    }


{-| Split state on a character returning a tuple containing:

1.  The state the the left of the character.
2.  The character the state was split on
3.  The state to the right of the character.

The state will be split on any of the list characters provided.
The first `n` occurrences are ignored.

`Nothing` is returned if `(n + 1)` characters from the list are not contained within the
source of the state.

-}
splitStateSkipping : Int -> List Char -> ParseState -> Maybe ( ParseState, Char, ParseState )
splitStateSkipping n chars ({ start, source } as state) =
    findNthOneOfHelper n chars "" state.source 0
        |> Maybe.map
            (\( left, splitChar, right, leftSize ) ->
                ( { source = left
                  , start = start
                  }
                , splitChar
                , { source = right
                  , start = start + leftSize + 1
                  }
                )
            )


{-| Remove whitespace and newlines from the beginning and end of state.

Should always trim in the same way as `String.trim` and will update `start` with the number of characters trimmed.

-}
trimState : ParseState -> ParseState
trimState =
    trimStart >> trimEnd


findNthOneOfHelper : Int -> List Char -> String -> String -> Int -> Maybe ( String, Char, String, Int )
findNthOneOfHelper n chars previousReversed source index =
    String.uncons source
        |> Maybe.andThen
            (\( first, rest ) ->
                if List.any (\c -> c == first) chars then
                    if n == 0 then
                        Just ( String.reverse previousReversed, first, rest, index )
                    else
                        findNthOneOfHelper (n - 1) chars (String.cons first previousReversed) rest (index + 1)
                else
                    findNthOneOfHelper n chars (String.cons first previousReversed) rest (index + 1)
            )


trimStart : ParseState -> ParseState
trimStart ({ source, start } as state) =
    let
        withoutFirstChar source =
            trimStart
                { state
                    | source = source
                    , start = start + 1
                }
    in
        case String.uncons source of
            Nothing ->
                state

            Just ( ' ', rest ) ->
                withoutFirstChar rest

            Just ( '\n', rest ) ->
                withoutFirstChar rest

            Just ( '\x0D', rest ) ->
                withoutFirstChar rest

            Just ( '\t', rest ) ->
                withoutFirstChar rest

            Just ( first, rest ) ->
                state


trimEnd : ParseState -> ParseState
trimEnd ({ source, start } as state) =
    { state
        | source = String.trimRight source
    }
