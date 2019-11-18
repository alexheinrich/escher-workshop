module Main exposing (main)

import Box exposing (..)
import Browser
import Figure exposing (george)
import Fishy exposing (fishShapes)
import Fitting exposing (createPicture)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events
import Letter exposing (..)
import Picture exposing (..)
import Rendering exposing (..)
import Svg exposing (Svg)


type Msg
    = UpdateAx String


type alias Model =
    { x : Float, y : Float }


placeInsideDiv : Model -> Svg Msg -> Html Msg
placeInsideDiv model svg =
    div [ style "padding" "50px" ] [ svg, range { min = 0, max = 300, value = model.x } ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = { x = 125.0, y = 75.0 }
        , view = view
        , update = update
        }


view : Model -> Html Msg
view model =
    let
        box =
            { a = model
            , b = { x = 250.0, y = 0.0 }
            , c = { x = 0.0, y = 250.0 }
            }
    in
    box
        |> createPicture george
        |> toSvgWithBoxes ( 500, 500 ) []
        |> placeInsideDiv model


update : Msg -> Model -> Model
update (UpdateAx newAx) model =
    let
        newX =
            String.toFloat newAx
                |> Maybe.withDefault model.x
    in
    { model | x = newX }


range : { min : Int, max : Int, value : Float } -> Html Msg
range { min, max, value } =
    div []
        [ input
            [ type_ "range"
            , Html.Attributes.min (String.fromInt min)
            , Html.Attributes.max (String.fromInt max)
            , Html.Attributes.value (String.fromFloat value)
            , Html.Events.onInput UpdateAx
            ]
            []
        , text (String.fromFloat value)
        ]
