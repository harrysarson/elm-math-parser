module ParserStateSpec exposing (tests)

import Char
import Expect
import Fuzz
import MaFuzz
import ParserState exposing (..)
import String
import Test exposing (describe, fuzz, fuzz2, test)


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
            , fuzz2 fuzzNonParenthesisChar MaFuzz.parseState "splitting on first character" <|
                \firstChar ({ source, start } as state) ->
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
            , fuzz Fuzz.int "respects nested parentheses" <|
                \start ->
                    { source = "The q(uick bro(wn fox jum)ps over the lazy d)og."
                    , start = start
                    }
                        |> splitStateSkipping 1 [ 'q', 'o' ]
                        |> Expect.equal
                            (Just
                                ( { source = "The q(uick bro(wn fox jum)ps over the lazy d)"
                                  , start = start
                                  }
                                , 'o'
                                , { source = "g."
                                  , start = start + 46
                                  }
                                )
                            )
            , fuzz Fuzz.int "be lenient with unmatched parentheses" <|
                \start ->
                    { source = "The q)uick b(rown fox jum)ps over the lazy dog."
                    , start = start
                    }
                        |> splitStateSkipping 1 [ 'u', 'o' ]
                        |> Expect.equal
                            (Just
                                ( { source = "The q)uick b(rown fox jum)ps "
                                  , start = start
                                  }
                                , 'o'
                                , { source = "ver the lazy dog."
                                  , start = start + 30
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
        |> Fuzz.map
            (\c ->
                if c == '(' then
                    ' '

                else
                    c
            )



{- |> Fuzz.conditional
   { retries = 10
   , fallback = Char.fromCode << (-) 1 << Char.toCode
   , condition = (\c -> c /= '(')
   }
-}
