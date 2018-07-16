module MaFuzz exposing (symbol, binaryOperator, spaces, addSpaces)

import Char
import Fuzz
import Random.Pcg as Random exposing (Generator)
import Expression


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
            ]
            |> Fuzz.map (Char.fromCode)


symbol : Fuzz.Fuzzer String
symbol =
    Fuzz.list symbolChar
        |> Fuzz.map2 (::) symbolChar
        |> Fuzz.map String.fromList


binaryOperator : Fuzz.Fuzzer Char
binaryOperator =
    Fuzz.oneOf <| List.map Fuzz.constant Expression.binaryOperators


spaces : Fuzz.Fuzzer String
spaces =
    Fuzz.list (Fuzz.constant ' ')
        |> Fuzz.map String.fromList


addSpaces : Fuzz.Fuzzer String -> Fuzz.Fuzzer String
addSpaces fuzz =
    spaces
        |> Fuzz.map2 String.append fuzz
        |> Fuzz.map2 String.append spaces
