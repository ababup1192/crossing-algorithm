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


type alias Gen =
    List Bool


type alias Model =
    {}


init : () -> ( Model, Cmd Msg )
init _ =
    ( {}, Cmd.none )



-- ---------------------------
-- UPDATE
-- ---------------------------


type alias Index =
    Int


type Msg
    = SwapGen Individual Index


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



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


beforeIndividualView : Individual -> Gen -> Html Msg
beforeIndividualView individual gene =
    ul [] <|
        (gene
            |> List.indexedMap
                (\idx g ->
                    li [ onClick <| SwapGen individual idx ] [ text <| bool2Text g ]
                )
        )


afterIndivisualView : Index -> Gen -> Html Msg
afterIndivisualView index gen =
    let
        base =
            gen |> List.take (index + 1)

        crossing =
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
                (crossing
                    |> List.map
                        (\g ->
                            li [] [ text <| bool2Text g ]
                        )
                )
            ]
        ]


view : Model -> Html Msg
view model =
    section [ class "crossing" ]
        [ article [ class "before" ]
            [ h1 [] [ text "対象個体 第1世代" ]
            , article []
                [ h2 [] [ text "個体A" ]
                , ul []
                    [ li [] [ text "0" ]
                    , li [] [ text "1" ]
                    , li [] [ text "1" ]
                    , li [] [ text "1" ]
                    , li [] [ text "1" ]
                    , li [] [ text "1" ]
                    , li [] [ text "1" ]
                    , li [] [ text "1" ]
                    , li [] [ text "1" ]
                    , li [] [ text "1" ]
                    ]
                ]
            , article []
                [ h2 [] [ text "個体B" ]
                , ul []
                    [ li [] [ text "0" ]
                    , li [] [ text "1" ]
                    , li [] [ text "0" ]
                    , li [] [ text "0" ]
                    , li [] [ text "0" ]
                    , li [] [ text "0" ]
                    , li [] [ text "0" ]
                    , li [] [ text "0" ]
                    , li [] [ text "0" ]
                    , li [] [ text "0" ]
                    ]
                ]
            ]
        , article []
            [ input [ type_ "button", value "世代交代" ] []
            ]
        , article [ class "after" ]
            [ h1 [] [ text "第2世代" ]
            , article []
                [ h2 [] [ text "個体A" ]
                , ul []
                    [ li []
                        [ ul []
                            [ li [] [ text "0" ]
                            , li [] [ text "1" ]
                            , li [] [ text "1" ]
                            , li [] [ text "1" ]
                            , li [] [ text "1" ]
                            ]
                        ]
                    , li []
                        [ ul []
                            [ li [] [ text "1" ]
                            , li [] [ text "1" ]
                            , li [] [ text "1" ]
                            , li [] [ text "1" ]
                            , li [] [ text "1" ]
                            ]
                        ]
                    ]
                ]
            , article []
                [ h2 [] [ text "個体B" ]
                , ul []
                    [ li []
                        [ ul []
                            [ li [] [ text "0" ]
                            , li [] [ text "1" ]
                            , li [] [ text "0" ]
                            , li [] [ text "0" ]
                            , li [] [ text "0" ]
                            ]
                        ]
                    , li []
                        [ ul []
                            [ li [] [ text "0" ]
                            , li [] [ text "0" ]
                            , li [] [ text "0" ]
                            , li [] [ text "0" ]
                            , li [] [ text "0" ]
                            ]
                        ]
                    ]
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
