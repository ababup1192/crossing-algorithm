module Tests exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (..)
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector


suite : Test
suite =
    describe "beforeIndividualView" <|
        let
            allFalseA =
                beforeIndividualView A [ False, False, False, False, False, False, False, False, False, False ]

            allTrueB =
                beforeIndividualView B [ True, True, True, True, True, True, True, True, True, True ]
        in
        [ test "遺伝子が全てFalseである" <|
            \() ->
                allFalseA
                    |> Query.fromHtml
                    |> Query.children [ Selector.text "0" ]
                    |> Query.count (Expect.equal 10)
        , test "遺伝子が全てTrueである" <|
            \() ->
                allTrueB
                    |> Query.fromHtml
                    |> Query.children [ Selector.text "1" ]
                    |> Query.count (Expect.equal 10)
        , test "個体Aの0番目を反転させる" <|
            \() ->
                allFalseA
                    |> Query.fromHtml
                    |> Query.findAll [ Selector.tag "li" ]
                    |> Query.index 0
                    |> Event.simulate Event.click
                    |> Event.expect (SwapGen A 0)
        , test "個体Bの1番目を反転させる" <|
            \() ->
                allTrueB
                    |> Query.fromHtml
                    |> Query.findAll [ Selector.tag "li" ]
                    |> Query.index 1
                    |> Event.simulate Event.click
                    |> Event.expect (SwapGen B 1)
        ]
