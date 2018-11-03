module Tests.Main exposing (tests)

import Main
import Test exposing (describe, test)
import Expect


tests : Test.Test
tests =
    describe "Main"
        [ test "it compiles" <|
            always Expect.pass
        ]