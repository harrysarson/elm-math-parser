module MaFuzz exposing (symbol, binaryOperator, unaryOperator, parseState, spaces, addSpaces)

import Char
import Set
import Fuzz
import Random.Pcg as Random exposing (Generator)
import Expression
import ParseState exposing (ParseState)


symbolChar : Fuzz.Fuzzer Char
symbolChar =
    let
        randomChar =
            Random.frequency
                [ ( 8, Random.int (Char.toCode 'a') (Char.toCode 'z') )
                , ( 4, Random.int (Char.toCode 'A') (Char.toCode 'Z') )
                , ( 1, Random.int (Char.toCode '0') (Char.toCode '9') )
                ]
                |> Random.map (Char.fromCode)

        lowerCase =
            Fuzz.intRange (Char.toCode 'a') (Char.toCode 'z')

        upperCase =
            Fuzz.intRange (Char.toCode 'A') (Char.toCode 'Z')

        number =
            Fuzz.intRange (Char.toCode '0') (Char.toCode '9')
    in
        Fuzz.frequency
            [ ( 8, lowerCase )
            , ( 4, upperCase )
            , ( 1, number )
            , ( 0.1, Fuzz.constant (Char.toCode '.'))
            ]
            |> Fuzz.map (Char.fromCode)


symbol : Fuzz.Fuzzer String
symbol =
    Fuzz.list symbolChar
        |> Fuzz.map2 (::) symbolChar
        |> Fuzz.map String.fromList


binaryOperator : Fuzz.Fuzzer Char
binaryOperator =
    Expression.binaryOperators
        |> List.map (Set.toList >> List.map Fuzz.constant)
        |> List.map Fuzz.oneOf
        |> Fuzz.oneOf


unaryOperator : Fuzz.Fuzzer Char
unaryOperator =
    Expression.unaryOperators
        |> Set.toList
        |> List.map Fuzz.constant
        |> Fuzz.oneOf


spaces : Fuzz.Fuzzer String
spaces =
    Fuzz.list (Fuzz.constant ' ')
        |> Fuzz.map String.fromList


addSpaces : Fuzz.Fuzzer String -> Fuzz.Fuzzer String
addSpaces fuzz =
    spaces
        |> Fuzz.map2 String.append fuzz
        |> Fuzz.map2 String.append spaces


parseState : Fuzz.Fuzzer ParseState
parseState =
    let
        sourceCharFuzz =
            Fuzz.frequency
                [ ( 2, Fuzz.constant '\n' )
                , ( 2, Fuzz.constant ' ' )
                , ( 1, Fuzz.constant '\x0D' )
                , ( 1, Fuzz.constant '\t' )
                , ( 10, Fuzz.char )
                ]

        sourceFuzz =
            Fuzz.list sourceCharFuzz
                |> Fuzz.map String.fromList
    in
        Fuzz.map2
            (\source start -> { source = source, start = start })
            sourceFuzz
            (Fuzz.intRange 0 1000)
