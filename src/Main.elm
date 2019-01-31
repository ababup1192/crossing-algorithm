module Main exposing
    ( CrossingMode(..)
    , Individual(..)
    , Model
    , Msg(..)
    , afterIndivisualView
    , beforeIndividualView
    , crossing
    , init
    , main
    , swapGene
    , twoPointCrossing
    , update
    , view
    )

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, type_, value)
import Html.Events exposing (..)
import Random



-- ---------------------------
-- MODEL
-- ---------------------------


type Individual
    = A
    | B


type alias Gene =
    List Bool


crossing : Index -> Gene -> Gene -> ( Gene, Gene )
crossing index gene1 gene2 =
    let
        baseGene1 =
            List.take (index + 1) gene1

        crossedGene1 =
            List.drop (index + 1) gene1

        baseGene2 =
            List.take (index + 1) gene2

        crossedGene2 =
            List.drop (index + 1) gene2
    in
    ( baseGene1 ++ crossedGene2, baseGene2 ++ crossedGene1 )


twoPointCrossing : Index -> Index -> Gene -> Gene -> ( Gene, Gene )
twoPointCrossing start end gene1 gene2 =
    let
        twoPointCrossing_ g1 g2 =
            List.take start g1 ++ (g2 |> List.drop start |> List.take (end - start)) ++ List.drop end g1
    in
    ( twoPointCrossing_ gene1 gene2, twoPointCrossing_ gene2 gene1 )


type CrossingMode
    = Crossing Index
    | TwoPointCrossing Index Index


mode2Text : CrossingMode -> String
mode2Text crossingMode =
    case crossingMode of
        Crossing _ ->
            "単交叉"

        TwoPointCrossing _ _ ->
            "2点交叉"


type alias Model =
    { geneA : Gene
    , geneB : Gene
    , crossingMode : CrossingMode
    , generation : Int
    , seed : Random.Seed
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( index, seed ) =
            Random.step genCrossingIndex (Random.initialSeed 999)

        firstCrosing =
            Crossing index
    in
    ( { geneA = []
      , geneB = []
      , crossingMode = firstCrosing
      , generation = 1
      , seed = seed
      }
    , Cmd.batch [ Random.generate GenGene genGene ]
    )


genGene : Random.Generator ( List Bool, List Bool )
genGene =
    let
        genGene_ =
            Random.list 10 (Random.int 0 1 |> Random.map (\n -> n == 0))
    in
    Random.pair genGene_ genGene_


genCrossingIndex : Random.Generator Int
genCrossingIndex =
    Random.int 1 8


genTwoCrossingIndex : Random.Generator ( Int, Int )
genTwoCrossingIndex =
    Random.int 0 9
        |> Random.andThen
            (\x ->
                let
                    list =
                        List.range 0 9 |> List.filter (\n -> n /= x)
                in
                case list of
                    [] ->
                        Random.constant ( -1, -1 )

                    h :: t ->
                        Random.uniform h t
                            |> Random.map
                                (\y -> ( min x y, max x y ))
            )


genCrossingSeparator : CrossingMode -> Cmd Msg
genCrossingSeparator crossingMode =
    case crossingMode of
        Crossing _ ->
            Random.generate GenCrossingSeparator <| (genCrossingIndex |> Random.map Crossing)

        TwoPointCrossing _ _ ->
            Random.generate GenCrossingSeparator <|
                (genTwoCrossingIndex |> Random.map (\( start, end ) -> TwoPointCrossing start end))



-- ---------------------------
-- UPDATE
-- ---------------------------


type alias Index =
    Int


type Msg
    = SwapGene Individual Index
    | GenerationalChange
    | GenCrossingSeparator CrossingMode
    | GenGene ( List Bool, List Bool )
    | SwapMode


swapGene : Index -> Gene -> Gene
swapGene index gene =
    let
        front =
            List.take index gene

        target =
            gene |> List.drop index |> List.take 1 |> List.map (\g -> not g)

        behind =
            List.drop (index + 1) gene
    in
    front ++ target ++ behind


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ geneA, geneB, crossingMode, generation, seed } as model) =
    case msg of
        SwapGene individual index ->
            case individual of
                A ->
                    ( { model | geneA = swapGene index geneA }, genCrossingSeparator crossingMode )

                B ->
                    ( { model | geneB = swapGene index geneB }, genCrossingSeparator crossingMode )

        GenerationalChange ->
            let
                ( nextGeneA, nextGeneB ) =
                    case crossingMode of
                        Crossing idx ->
                            crossing idx geneA geneB

                        TwoPointCrossing idx1 idx2 ->
                            twoPointCrossing idx1 idx2 geneA geneB
            in
            ( { model | geneA = nextGeneA, geneB = nextGeneB, generation = generation + 1 }, genCrossingSeparator crossingMode )

        GenCrossingSeparator crgMode ->
            ( { model | crossingMode = crgMode }, Cmd.none )

        SwapMode ->
            case crossingMode of
                Crossing _ ->
                    let
                        ( nextIndexPair, nextSeed ) =
                            Random.step genTwoCrossingIndex seed

                        ( start, end ) =
                            nextIndexPair
                    in
                    ( { model | crossingMode = TwoPointCrossing start end, seed = nextSeed }, Cmd.none )

                TwoPointCrossing _ _ ->
                    let
                        ( nextIndex, nextSeed ) =
                            Random.step genCrossingIndex seed
                    in
                    ( { model | crossingMode = Crossing nextIndex, seed = nextSeed }, Cmd.none )

        GenGene ( gA, gB ) ->
            ( { model | geneA = gA, geneB = gB }, Cmd.none )



-- ---------------------------
-- VIEW
-- ---------------------------


bool2Text : Bool -> String
bool2Text b =
    case b of
        True ->
            "1"

        False ->
            "0"


beforeIndividualView : Individual -> Gene -> Html Msg
beforeIndividualView individual gene =
    ul [] <|
        (gene
            |> List.indexedMap
                (\idx g ->
                    li [ onClick <| SwapGene individual idx ] [ text <| bool2Text g ]
                )
        )


afterIndivisualView : CrossingMode -> Gene -> Html Msg
afterIndivisualView crossingMode gene =
    case crossingMode of
        Crossing index ->
            let
                base =
                    gene |> List.take (index + 1)

                crossed =
                    gene |> List.drop (index + 1)
            in
            ul []
                [ li [ class "chunk-base" ]
                    [ ul [] <|
                        (base
                            |> List.map
                                (\g ->
                                    li [] [ text <| bool2Text g ]
                                )
                        )
                    ]
                , li [ class "chunk-target" ]
                    [ ul [] <|
                        (crossed
                            |> List.map
                                (\g ->
                                    li [] [ text <| bool2Text g ]
                                )
                        )
                    ]
                ]

        TwoPointCrossing start end ->
            let
                baseStart =
                    List.take start gene

                target =
                    gene |> List.drop start |> List.take (end - start)

                baseEnd =
                    List.drop end gene
            in
            ul []
                [ li [ class "chunk-base" ]
                    [ ul [] <|
                        (baseStart
                            |> List.map
                                (\g ->
                                    li [] [ text <| bool2Text g ]
                                )
                        )
                    ]
                , li [ class "chunk-target" ]
                    [ ul [] <|
                        (target
                            |> List.map
                                (\g ->
                                    li [] [ text <| bool2Text g ]
                                )
                        )
                    ]
                , li [ class "chunk-base" ]
                    [ ul [] <|
                        (baseEnd
                            |> List.map
                                (\g ->
                                    li [] [ text <| bool2Text g ]
                                )
                        )
                    ]
                ]


view : Model -> Html Msg
view { geneA, geneB, crossingMode, generation } =
    let
        ( newGene1, newGene2 ) =
            case crossingMode of
                Crossing index ->
                    crossing index geneA geneB

                TwoPointCrossing index1 index2 ->
                    twoPointCrossing index1 index2 geneA geneB
    in
    section [ class "crossing" ]
        [ article [ class "before" ]
            [ h1 [] [ text <| "対象個体 第" ++ String.fromInt generation ++ "世代" ]
            , article []
                [ h2 [] [ text "個体A" ]
                , beforeIndividualView A geneA
                ]
            , article []
                [ h2 [] [ text "個体B" ]
                , beforeIndividualView B geneB
                ]
            ]
        , article []
            [ input [ type_ "button", value "世代交代", class "pure-button pure-button-primary", onClick GenerationalChange ] []
            , input [ type_ "button", value <| mode2Text crossingMode, class "pure-button pure-button-primary", onClick SwapMode ] []
            ]
        , article [ class "after" ]
            [ h1 [] [ text <| "第" ++ String.fromInt (generation + 1) ++ "世代" ]
            , article []
                [ h2 [] [ text "個体A" ]
                , afterIndivisualView crossingMode newGene1
                ]
            , article []
                [ h2 [] [ text "個体B" ]
                , afterIndivisualView crossingMode newGene2
                ]
            ]
        ]



-- ---------------------------
-- MAIN
-- ---------------------------


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view =
            \m ->
                { title = "交叉アルゴリズム"
                , body = [ view m ]
                }
        , subscriptions = \_ -> Sub.none
        }
