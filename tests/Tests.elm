module Tests exposing (unitTest)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (..)
import Test exposing (..)


unitTest : Test
unitTest =
    describe "reverse"
        [ test "abc -> cba" <|
            \() ->
                String.reverse "abc"
                    |> Expect.equal "cba"
        ]
