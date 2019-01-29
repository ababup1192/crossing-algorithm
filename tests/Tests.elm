module Tests exposing (afterIndividualViewTest, beforeIndividualViewTest, crossingTest, swapGeneTest)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (..)
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector


swapGeneTest : Test
swapGeneTest =
    describe "swapGeneTest" <|
        let
            gene =
                List.repeat 10 True
        in
        [ test "遺伝子の先頭のビットをひっくり返すと、先頭だけがFalseの遺伝子ができあがる" <|
            \() ->
                swapGene 0 gene
                    |> Expect.equal (False :: List.repeat 9 True)
        , test "遺伝子の末尾のビットをひっくり返すと、末尾だけがFalseの遺伝子ができあがる" <|
            \() ->
                swapGene 9 gene
                    |> Expect.equal (List.repeat 9 True ++ [ False ])
        ]


crossingTest : Test
crossingTest =
    describe "crossingTest" <|
        let
            geneA =
                List.repeat 5 True ++ List.repeat 5 False

            geneB =
                List.repeat 5 False ++ List.repeat 5 True
        in
        [ test "geneAとgeneBを真ん中で交叉すると、全てがTrueの次世代geneAと全てがFalseの次世代geneBができる" <|
            \() ->
                crossing 4 geneA geneB
                    |> Expect.equal
                        ( List.repeat 10 True
                        , List.repeat 10 False
                        )
        ]


beforeIndividualViewTest : Test
beforeIndividualViewTest =
    describe "beforeIndividualView" <|
        let
            allFalseA =
                beforeIndividualView A <| List.repeat 10 False

            allTrueB =
                beforeIndividualView B <| List.repeat 10 True
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
                afterIndivisualView 4 <| List.repeat 5 False ++ List.repeat 5 True

            twoEightTrueFalseBits =
                afterIndivisualView 1 <| List.repeat 2 True ++ List.repeat 8 False
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
