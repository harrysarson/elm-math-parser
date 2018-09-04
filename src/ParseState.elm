module ParseState exposing
    ( ParseState
    , splitStateSkipping, trimState
    )

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
    findNthOneOfHelper n chars 0 "" state.source 0
        |> Maybe.map
            (\{ left, splitChar, right, leftSize } ->
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


findClosingParenthesis : String -> String -> Int -> Maybe ( String, Int )
findClosingParenthesis previousReversed source index =
    String.uncons source
        |> Maybe.andThen
            (\( possiblyCloseParenthesis, rest ) ->
                if possiblyCloseParenthesis == ')' then
                    Just ( String.reverse previousReversed, index )

                else
                    findClosingParenthesis (String.cons possiblyCloseParenthesis previousReversed) rest (index + 1)
            )



-- todo: missing open or closing parenthesis
-- todo: rename type and function


type alias FindResult =
    { left : String
    , splitChar : Char
    , right : String
    , leftSize : Int
    }


findNthOneOfHelper : Int -> List Char -> Int -> String -> String -> Int -> Maybe FindResult
findNthOneOfHelper n chars closesRequired previousReversed source index =
    String.uncons source
        |> Maybe.andThen
            (\( first, rest ) ->
                case closesRequired of
                    0 ->
                        if first == '(' then
                            findNthOneOfHelper n chars 1 (String.cons first previousReversed) rest (index + 1)

                        else if List.any (\c -> c == first) chars then
                            if n == 0 then
                                Just
                                    { left = String.reverse previousReversed
                                    , splitChar = first
                                    , right = rest
                                    , leftSize = index
                                    }

                            else
                                findNthOneOfHelper (n - 1) chars 0 (String.cons first previousReversed) rest (index + 1)

                        else
                            findNthOneOfHelper n chars 0 (String.cons first previousReversed) rest (index + 1)

                    _ ->
                        case first of
                            ')' ->
                                findNthOneOfHelper n chars (closesRequired - 1) (String.cons first previousReversed) rest (index + 1)

                            '(' ->
                                findNthOneOfHelper n chars (closesRequired + 1) (String.cons first previousReversed) rest (index + 1)

                            _ ->
                                findNthOneOfHelper n chars closesRequired (String.cons first previousReversed) rest (index + 1)
            )


trimStart : ParseState -> ParseState
trimStart ({ source, start } as state) =
    case String.uncons source of
        Nothing ->
            state

        Just ( ' ', rest ) ->
            trimStart
                { source = rest
                , start = start + 1
                }

        Just ( '\n', rest ) ->
            trimStart
                { source = rest
                , start = start + 1
                }

        Just ( '\u{000D}', rest ) ->
            trimStart
                { source = rest
                , start = start + 1
                }

        Just ( '\t', rest ) ->
            trimStart
                { source = rest
                , start = start + 1
                }

        Just ( first, rest ) ->
            state


trimEnd : ParseState -> ParseState
trimEnd ({ source, start } as state) =
    { state
        | source = String.trimRight source
    }
