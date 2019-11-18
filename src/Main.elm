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
    = Update ABC Direction String


type Direction
    = X
    | Y


type ABC
    = A
    | B
    | C


type alias Point =
    { x : Float, y : Float }


type alias Model =
    { a : Point, b : Point, c : Point }


placeInsideDiv : Model -> Svg Msg -> Html Msg
placeInsideDiv model svg =
    div [ style "padding" "50px" ]
        [ svg
        , rangeXY { label = "A", model = model.a, msg = Update A }
        , rangeXY { label = "B", model = model.b, msg = Update B }
        , rangeXY { label = "C", model = model.c, msg = Update C }
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = { a = { x = 125.0, y = 75.0 }, b = { x = 250.0, y = 0.0 }, c = { x = 0.0, y = 250.0 } }
        , view = view
        , update = update
        }


view : Model -> Html Msg
view model =
    let
        box =
            { a = model.a
            , b = model.b
            , c = model.c
            }
    in
    box
        |> createPicture george
        |> toSvgWithBoxes ( 500, 500 ) []
        |> placeInsideDiv model


update : Msg -> Model -> Model
update (Update abc direction newVal) model =
    let
        new =
            String.toFloat newVal
                |> Maybe.withDefault 0
    in
    case abc of
        A ->
            { model | a = updatePoint model.a direction new }

        B ->
            { model | b = updatePoint model.b direction new }

        C ->
            { model | c = updatePoint model.c direction new }


updatePoint : Point -> Direction -> Float -> Point
updatePoint point direction new =
    case direction of
        X ->
            { point | x = new }

        Y ->
            { point | y = new }


range : { min : Int, max : Int, value : Float, label : String, msg : String -> Msg } -> Html Msg
range { min, max, value, label, msg } =
    div []
        [ text label
        , input
            [ type_ "range"
            , Html.Attributes.min (String.fromInt min)
            , Html.Attributes.max (String.fromInt max)
            , Html.Attributes.value (String.fromFloat value)
            , Html.Events.onInput msg
            ]
            []
        , text (String.fromFloat value)
        ]


rangeXY : { label : String, model : Point, msg : Direction -> String -> Msg } -> Html Msg
rangeXY { label, model, msg } =
    div []
        [ text label
        , range { min = -1000, max = 1000, value = model.x, label = "X", msg = msg X }
        , range { min = -1000, max = 1000, value = model.y, label = "Y", msg = msg Y }
        ]
