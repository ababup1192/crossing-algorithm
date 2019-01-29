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
    { geneA : Gene, geneB : Gene, crossingMode : CrossingMode, generation : Int }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        defaultCrossingMode =
            Crossing -1
    in
    ( { geneA = [ False, False, False, False, False, False, False, False, False, False ]
      , geneB = [ False, False, False, False, False, False, False, False, False, False ]
      , crossingMode = defaultCrossingMode
      , generation = 1
      }
    , genCrossingSeparator defaultCrossingMode
    )


genCrossingSeparator : CrossingMode -> Cmd Msg
genCrossingSeparator crossingMode =
    case crossingMode of
        Crossing _ ->
            Random.generate GenCrossingSeparator <| (Random.int 1 8 |> Random.map Crossing)

        TwoPointCrossing _ _ ->
            Random.generate GenCrossingSeparator <|
                (Random.int 0 9
                    |> Random.andThen
                        (\x ->
                            let
                                list =
                                    List.range 0 9 |> List.filter (\n -> n /= x)

                                h =
                                    List.head list |> Maybe.withDefault -1

                                t =
                                    List.drop 1 list
                            in
                            Random.uniform h t |> Random.map (\y -> TwoPointCrossing (min x y) (max x y))
                        )
                )



-- ---------------------------
-- UPDATE
-- ---------------------------


type alias Index =
    Int


type Msg
    = SwapGene Individual Index
    | GenerationalChange
    | GenCrossingSeparator CrossingMode
    | SwapMode


swapGene : Index -> Gene -> Gene
swapGene index gene =
    let
        b =
            List.take index gene

        target =
            gene |> List.drop index |> List.take 1 |> List.map (\g -> not g)

        a =
            List.drop (index + 1) gene
    in
    b ++ target ++ a


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ geneA, geneB, crossingMode, generation } as model) =
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
            let
                nextMode =
                    case crossingMode of
                        Crossing _ ->
                            TwoPointCrossing -1 -1

                        TwoPointCrossing _ _ ->
                            Crossing -1
            in
            ( { model | crossingMode = nextMode, generation = 1 }, genCrossingSeparator nextMode )



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
