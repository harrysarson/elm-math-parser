module Tests.Main exposing (tests)

import Expect
import Main
import Test exposing (describe, test)


tests : Test.Test
tests =
    describe "Main"
        [ test "it compiles" <|
            always Expect.pass
        ]
