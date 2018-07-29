module ParseStateSpec exposing (tests)

import String
import Char
import ParseState exposing (..)
import Test exposing (test, describe, fuzz)
import Fuzz
import Expect
import MaFuzz


tests : Test.Test
tests =
    describe "parse state"
        [ describe "splitStateOneOf"
            [ fuzz (Fuzz.tuple3 ( Fuzz.int, Fuzz.intRange 0 10, Fuzz.list Fuzz.char )) "empty source string gives Nothing" <|
                \( start, n, chars ) ->
                    { source = ""
                    , start = start
                    }
                        |> splitStateSkipping n chars
                        |> Expect.equal Nothing
            , fuzz Fuzz.int "splitting on first instance of character" <|
                \start ->
                    { source = "The quick brown fox jumps over the lazy dog."
                    , start = start
                    }
                        |> splitStateSkipping 0 [ 'h' ]
                        |> Expect.equal
                            (Just
                                ( { source = "T"
                                  , start = start
                                  }
                                , 'h'
                                , { source = "e quick brown fox jumps over the lazy dog."
                                  , start = start + 2
                                  }
                                )
                            )
            , fuzz Fuzz.int "splitting on second instance of one character" <|
                \start ->
                    { source = "The quick brown fox jumps over the lazy dog."
                    , start = start
                    }
                        |> splitStateSkipping 1 [ 'o' ]
                        |> Expect.equal
                            (Just
                                ( { source = "The quick brown f"
                                  , start = start
                                  }
                                , 'o'
                                , { source = "x jumps over the lazy dog."
                                  , start = start + 18
                                  }
                                )
                            )
            , fuzz Fuzz.int "splitting on second instance of two characters" <|
                \start ->
                    { source = "The quick brown fox jumps over the lazy dog."
                    , start = start
                    }
                        |> splitStateSkipping 1 [ 'o', 'q' ]
                        |> Expect.equal
                            (Just
                                ( { source = "The quick br"
                                  , start = start
                                  }
                                , 'o'
                                , { source = "wn fox jumps over the lazy dog."
                                  , start = start + 13
                                  }
                                )
                            )
            , fuzz (Fuzz.tuple ( fuzzNonParenthesisChar, MaFuzz.parseState )) "splitting on first character" <|
                \( firstChar, { source, start } as state ) ->
                    let
                        listOfAscii =
                            List.range 0 255
                                |> List.map Char.fromCode
                    in
                        { state | source = String.cons firstChar source }
                            |> splitStateSkipping 0 listOfAscii
                            |> Expect.equal
                                (Just
                                    ( { source = ""
                                      , start = start
                                      }
                                    , firstChar
                                    , { source = source
                                      , start = start + 1
                                      }
                                    )
                                )
            , fuzz Fuzz.int "respects parentheses" <|
                \start ->
                    { source = "The (quick brown fox jumps) over the lazy dog."
                    , start = start
                    }
                        |> splitStateSkipping 0 [ 'q', 'o' ]
                        |> Expect.equal
                            (Just
                                ( { source = "The (quick brown fox jumps) "
                                  , start = start
                                  }
                                , 'o'
                                , { source = "ver the lazy dog."
                                  , start = start + 29
                                  }
                                )
                            )
            , fuzz Fuzz.int "ignores every character after opening parentheses" <|
                \start ->
                    { source = "Th(e quick brown fox jumps over the lazy dog."
                    , start = start
                    }
                        |> splitStateSkipping 0 [ 'q', 'e' ]
                        |> Expect.equal Nothing
            , fuzz Fuzz.int "splits on character before an opening parentheses" <|
                \start ->
                    { source = "The q(uick brown fox jumps over the lazy dog."
                    , start = start
                    }
                        |> splitStateSkipping 0 [ 'q', 'o' ]
                        |> Expect.equal
                            (Just
                                ( { source = "The "
                                  , start = start
                                  }
                                , 'q'
                                , { source = "(uick brown fox jumps over the lazy dog."
                                  , start = start + 5
                                  }
                                )
                            )
            ]
        , describe "trim state"
            [ fuzz MaFuzz.parseState "agrees with String.trim" <|
                \({ source } as state) ->
                    state
                        |> trimState
                        |> .source
                        |> Expect.equal (String.trim source)
            ]
        ]


fuzzNonParenthesisChar : Fuzz.Fuzzer Char
fuzzNonParenthesisChar =
    Fuzz.char
        |> Fuzz.conditional
            { retries = 10
            , fallback = Char.fromCode << (-) 1 << Char.toCode
            , condition = (\c -> c /= '(')
            }
