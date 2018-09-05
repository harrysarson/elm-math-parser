module MaFuzz exposing (addSpaces, binaryOperator, parseState, spaces, symbol, unaryOperator)

import Char
import Fuzz
import MathExpression
import ParserState exposing (ParserState)
import Random exposing (Generator)
import Random.Extra
import Set


symbolChar : Fuzz.Fuzzer Char
symbolChar =
    let
        randomChar =
            Random.Extra.frequency
                ( 8, Random.int (Char.toCode 'a') (Char.toCode 'z') )
                [ ( 4, Random.int (Char.toCode 'A') (Char.toCode 'Z') )
                , ( 1, Random.int (Char.toCode '0') (Char.toCode '9') )
                ]
                |> Random.map Char.fromCode

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
        , ( 0.1, Fuzz.constant (Char.toCode '.') )
        ]
        |> Fuzz.map Char.fromCode


symbol : Fuzz.Fuzzer String
symbol =
    Fuzz.list symbolChar
        |> Fuzz.map2 (::) symbolChar
        |> Fuzz.map String.fromList


binaryOperator : Fuzz.Fuzzer MathExpression.BinaryOperator
binaryOperator =
    [ MathExpression.Add
    , MathExpression.Subtract
    , MathExpression.Multiply
    , MathExpression.Divide
    ]
        |> List.map Fuzz.constant
        |> Fuzz.oneOf


unaryOperator : Fuzz.Fuzzer MathExpression.UnaryOperator
unaryOperator =
    [ MathExpression.UnaryAdd
    , MathExpression.UnarySubtract
    ]
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


parseState : Fuzz.Fuzzer ParserState
parseState =
    let
        sourceCharFuzz =
            Fuzz.frequency
                [ ( 2, Fuzz.constant '\n' )
                , ( 2, Fuzz.constant ' ' )
                , ( 1, Fuzz.constant '\u{000D}' )
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
