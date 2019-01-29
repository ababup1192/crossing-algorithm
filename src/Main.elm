module Main exposing
    ( Individual(..)
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
import Html.Attributes exposing (..)
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


type alias Model =
    { geneA : Gene, geneB : Gene, separator : Index, generation : Int }


genCrossingSeparator : Cmd Msg
genCrossingSeparator =
    Random.generate GenCrossingSeparator <| Random.int 1 8


init : () -> ( Model, Cmd Msg )
init _ =
    ( { geneA = [ False, False, False, False, False, False, False, False, False, False ]
      , geneB = [ False, False, False, False, False, False, False, False, False, False ]
      , separator = 0
      , generation = 1
      }
    , genCrossingSeparator
    )



-- ---------------------------
-- UPDATE
-- ---------------------------


type alias Index =
    Int


type Msg
    = SwapGene Individual Index
    | GenerationalChange
    | GenCrossingSeparator Index


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
update msg ({ geneA, geneB, separator, generation } as model) =
    case msg of
        SwapGene individual index ->
            case individual of
                A ->
                    ( { model | geneA = swapGene index geneA }, genCrossingSeparator )

                B ->
                    ( { model | geneB = swapGene index geneB }, genCrossingSeparator )

        GenerationalChange ->
            let
                ( nextGeneA, nextGeneB ) =
                    crossing separator geneA geneB
            in
            ( { model | geneA = nextGeneA, geneB = nextGeneB, generation = generation + 1 }, genCrossingSeparator )

        GenCrossingSeparator idx ->
            ( { model | separator = idx }, Cmd.none )



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


afterIndivisualView : Index -> Gene -> Html Msg
afterIndivisualView index gen =
    let
        base =
            gen |> List.take (index + 1)

        crossed =
            gen |> List.drop (index + 1)
    in
    ul []
        [ li [ class "chunk" ]
            [ ul [] <|
                (base
                    |> List.map
                        (\g ->
                            li [] [ text <| bool2Text g ]
                        )
                )
            ]
        , li [ class "chunk" ]
            [ ul [] <|
                (crossed
                    |> List.map
                        (\g ->
                            li [] [ text <| bool2Text g ]
                        )
                )
            ]
        ]


view : Model -> Html Msg
view { geneA, geneB, separator, generation } =
    let
        ( nextGeneA, nextGeneB ) =
            crossing separator geneA geneB
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
            ]
        , article [ class "after" ]
            [ h1 [] [ text <| "第" ++ String.fromInt (generation + 1) ++ "世代" ]
            , article []
                [ h2 [] [ text "個体A" ]
                , afterIndivisualView separator nextGeneA
                ]
            , article []
                [ h2 [] [ text "個体B" ]
                , afterIndivisualView separator nextGeneB
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
