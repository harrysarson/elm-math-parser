module ParserStateSpec exposing (tests)

import Char
import Dict
import Expect
import Fuzz
import MathFuzz
import ParserState exposing (..)
import String
import Test exposing (describe, fuzz, fuzz2, test)


type Dummy
    = Dummy


tests : Test.Test
tests =
    describe "parse state"
        [ describe "splitStateOneOf"
            [ fuzz (Fuzz.tuple3 ( Fuzz.int, Fuzz.intRange 0 10, Fuzz.list Fuzz.char )) "empty source string gives Nothing" <|
                \( start, n, chars ) ->
                    let
                        charDict =
                            chars
                                |> List.map (\x -> ( x, x ))
                                |> Dict.fromList
                    in
                    { source = ""
                    , start = start
                    }
                        |> splitStateSkipping n charDict
                        |> Expect.equal Nothing
            , fuzz Fuzz.int "splitting on first instance of character" <|
                \start ->
                    { source = "The quick brown fox jumps over the lazy dog."
                    , start = start
                    }
                        |> splitStateSkipping 0 (Dict.singleton 'h' Dummy)
                        |> Expect.equal
                            (Just
                                ( { source = "T"
                                  , start = start
                                  }
                                , Dummy
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
                        |> splitStateSkipping 1 (Dict.singleton 'o' 235)
                        |> Expect.equal
                            (Just
                                ( { source = "The quick brown f"
                                  , start = start
                                  }
                                , 235
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
                        |> splitStateSkipping 1 (Dict.singleton 'o' "o" |> Dict.insert 'q' "q")
                        |> Expect.equal
                            (Just
                                ( { source = "The quick br"
                                  , start = start
                                  }
                                , "o"
                                , { source = "wn fox jumps over the lazy dog."
                                  , start = start + 13
                                  }
                                )
                            )
            , fuzz2 fuzzNonParenthesisChar MathFuzz.parseState "splitting on first character" <|
                \firstChar ({ source, start } as state) ->
                    let
                        listOfAscii =
                            List.range 0 255
                                |> List.map (\x -> ( Char.fromCode x, x ))
                                |> Dict.fromList
                    in
                    { state | source = String.cons firstChar source }
                        |> splitStateSkipping 0 listOfAscii
                        |> Expect.equal
                            (Just
                                ( { source = ""
                                  , start = start
                                  }
                                , Char.toCode firstChar
                                , { source = source
                                  , start = start + 1
                                  }
                                )
                            )
            , describe "round parentheses" <| parenthesesTests "(" ")"
            , describe "square parentheses" <| parenthesesTests "[" "]"
            , fuzz Fuzz.int "mixed parentheses" <|
                \start ->
                    { source = "The q(uick bro[wn fox jum]ps over the lazy d)og."
                    , start = start
                    }
                        |> splitStateSkipping 1 (Dict.singleton 'o' 'o' |> Dict.insert 'q' 'q')
                        |> Expect.equal
                            (Just
                                ( { source = "The q(uick bro[wn fox jum]ps over the lazy d)"
                                  , start = start
                                  }
                                , 'o'
                                , { source = "g."
                                  , start = start + 46
                                  }
                                )
                            )
            ]
        , describe "trim state"
            [ fuzz MathFuzz.parseState "agrees with String.trim" <|
                \({ source } as state) ->
                    state
                        |> trimState
                        |> .source
                        |> Expect.equal (String.trim source)
            ]
        ]


parenthesesTests : String -> String -> List Test.Test
parenthesesTests open close =
    [ fuzz Fuzz.int "respects parentheses" <|
        \start ->
            { source = "The " ++ open ++ "quick brown fox jumps" ++ close ++ " over the lazy dog."
            , start = start
            }
                |> splitStateSkipping 0 (Dict.singleton 'o' 'o' |> Dict.insert 'q' 'q')
                |> Expect.equal
                    (Just
                        ( { source = "The " ++ open ++ "quick brown fox jumps" ++ close ++ " "
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
            { source = "Th" ++ open ++ "e quick brown fox jumps over the lazy dog."
            , start = start
            }
                |> splitStateSkipping 0 (Dict.singleton 'e' 'e' |> Dict.insert 'q' 'q')
                |> Expect.equal Nothing
    , fuzz Fuzz.int "splits on character before an opening parentheses" <|
        \start ->
            { source = "The q" ++ open ++ "uick brown fox jumps over the lazy dog."
            , start = start
            }
                |> splitStateSkipping 0 (Dict.singleton 'o' 'o' |> Dict.insert 'q' 'q')
                |> Expect.equal
                    (Just
                        ( { source = "The "
                          , start = start
                          }
                        , 'q'
                        , { source = "" ++ open ++ "uick brown fox jumps over the lazy dog."
                          , start = start + 5
                          }
                        )
                    )
    , fuzz Fuzz.int "respects nested parentheses" <|
        \start ->
            { source = "The q" ++ open ++ "uick bro" ++ open ++ "wn fox jum" ++ close ++ "ps over the lazy d" ++ close ++ "og."
            , start = start
            }
                |> splitStateSkipping 1 (Dict.singleton 'o' 'o' |> Dict.insert 'q' 'q')
                |> Expect.equal
                    (Just
                        ( { source = "The q" ++ open ++ "uick bro" ++ open ++ "wn fox jum" ++ close ++ "ps over the lazy d" ++ close ++ ""
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
            { source = "The q" ++ close ++ "uick b" ++ open ++ "rown fox jum" ++ close ++ "ps over the lazy dog."
            , start = start
            }
                |> splitStateSkipping 1 (Dict.singleton 'o' 'o' |> Dict.insert 'u' 'u')
                |> Expect.equal
                    (Just
                        ( { source = "The q" ++ close ++ "uick b" ++ open ++ "rown fox jum" ++ close ++ "ps "
                          , start = start
                          }
                        , 'o'
                        , { source = "ver the lazy dog."
                          , start = start + 30
                          }
                        )
                    )
    ]


fuzzNonParenthesisChar : Fuzz.Fuzzer Char
fuzzNonParenthesisChar =
    Fuzz.char
        |> Fuzz.map
            (\c ->
                if c == '(' || c == '[' then
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
