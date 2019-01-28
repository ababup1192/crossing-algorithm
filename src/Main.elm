module Main exposing
    ( Individual(..)
    , Model
    , Msg(..)
    , afterIndivisualView
    , beforeIndividualView
    , init
    , main
    , update
    , view
    )

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



-- ---------------------------
-- MODEL
-- ---------------------------


type Individual
    = A
    | B


type alias Gene =
    List Bool


type alias Model =
    { geneA : Gene, geneB : Gene }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { geneA = [ False, False, False, False, False, False, False, False, False, False ]
      , geneB = [ False, False, False, False, False, False, False, False, False, False ]
      }
    , Cmd.none
    )



-- ---------------------------
-- UPDATE
-- ---------------------------


type alias Index =
    Int


type Msg
    = SwapGene Individual Index


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
update msg ({ geneA, geneB } as model) =
    case msg of
        SwapGene individual index ->
            case individual of
                A ->
                    ( { model | geneA = swapGene index geneA }, Cmd.none )

                B ->
                    ( { model | geneB = swapGene index geneB }, Cmd.none )



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
view { geneA, geneB } =
    section [ class "crossing" ]
        [ article [ class "before" ]
            [ h1 [] [ text "対象個体 第1世代" ]
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
            [ input [ type_ "button", value "世代交代" ] []
            ]
        , article [ class "after" ]
            [ h1 [] [ text "第2世代" ]
            , article []
                [ h2 [] [ text "個体A" ]
                , afterIndivisualView 4 [ False, False, False, False, False, True, True, True, True, True ]
                ]
            , article []
                [ h2 [] [ text "個体B" ]
                , afterIndivisualView 1 [ True, True, False, False, False, False, False, False, False, False ]
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
