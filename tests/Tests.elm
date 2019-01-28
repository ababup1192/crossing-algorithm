module Tests exposing (afterIndividualViewTest, beforeIndividualViewTest)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (..)
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector


beforeIndividualViewTest : Test
beforeIndividualViewTest =
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
                    |> Event.expect (SwapGene A 0)
        , test "個体Bの1番目を反転させる" <|
            \() ->
                allTrueB
                    |> Query.fromHtml
                    |> Query.findAll [ Selector.tag "li" ]
                    |> Query.index 1
                    |> Event.simulate Event.click
                    |> Event.expect (SwapGene B 1)
        ]


afterIndividualViewTest : Test
afterIndividualViewTest =
    describe "afterIndivisualView" <|
        let
            halfFalseTrueBits =
                afterIndivisualView 4 [ False, False, False, False, False, True, True, True, True, True ]

            twoEightTrueFalseBits =
                afterIndivisualView 1 [ True, True, False, False, False, False, False, False, False, False ]
        in
        [ test "遺伝子のベースは5個のFalseから出来ている" <|
            \() ->
                halfFalseTrueBits
                    |> Query.fromHtml
                    |> Query.findAll [ Selector.class "chunk" ]
                    |> Query.first
                    |> Query.findAll [ Selector.text "0" ]
                    |> Query.count (Expect.equal 5)
        , test "交叉後の遺伝子は5個のTrueから出来ている" <|
            \() ->
                halfFalseTrueBits
                    |> Query.fromHtml
                    |> Query.findAll [ Selector.class "chunk" ]
                    |> Query.index 1
                    |> Query.findAll [ Selector.text "1" ]
                    |> Query.count (Expect.equal 5)
        , test "遺伝子のベースは2個のTrueから出来ている" <|
            \() ->
                twoEightTrueFalseBits
                    |> Query.fromHtml
                    |> Query.findAll [ Selector.class "chunk" ]
                    |> Query.first
                    |> Query.findAll [ Selector.text "1" ]
                    |> Query.count (Expect.equal 2)
        , test "交叉後の遺伝子は8個のFalseから出来ている" <|
            \() ->
                twoEightTrueFalseBits
                    |> Query.fromHtml
                    |> Query.findAll [ Selector.class "chunk" ]
                    |> Query.index 1
                    |> Query.findAll [ Selector.text "0" ]
                    |> Query.count (Expect.equal 8)
        ]
