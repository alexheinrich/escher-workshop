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
import Vector exposing (Vector)


type Msg
    = Update ABC Axis String
    | Reset


type Axis
    = X
    | Y


type ABC
    = A
    | B
    | C


type alias Model =
    Box


initialModel : Model
initialModel =
    { a = { x = 125.0, y = 75.0 }, b = { x = 250.0, y = 0.0 }, c = { x = 0.0, y = 250.0 } }


placeInsideDiv : Model -> Svg Msg -> Html Msg
placeInsideDiv model svg =
    div [ style "padding" "50px" ]
        [ svg
        , rangeXY { label = "A (Origin)", model = model.a, msg = Update A }
        , rangeXY { label = "B", model = model.b, msg = Update B }
        , rangeXY { label = "C", model = model.c, msg = Update C }
        , resetButton { label = "Reset", msg = Reset }
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
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

        picture =
            createPicture fLetter
    in
    box
        |> above (flip picture) picture
        |> toSvgWithBoxes ( 500, 500 ) []
        |> placeInsideDiv model


update : Msg -> Model -> Model
update msg model =
    case msg of
        Update abc axis newVal ->
            let
                new =
                    String.toFloat newVal
                        |> Maybe.withDefault 0
            in
            case abc of
                A ->
                    { model | a = updateVector model.a axis new }

                B ->
                    { model | b = updateVector model.b axis new }

                C ->
                    { model | c = updateVector model.c axis new }

        Reset ->
            initialModel


updateVector : Vector -> Axis -> Float -> Vector
updateVector vector axis new =
    case axis of
        X ->
            { vector | x = new }

        Y ->
            { vector | y = new }


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


rangeXY : { label : String, model : Vector, msg : Axis -> String -> Msg } -> Html Msg
rangeXY { label, model, msg } =
    div []
        [ text label
        , range { min = -1000, max = 1000, value = model.x, label = "X", msg = msg X }
        , range { min = -1000, max = 1000, value = model.y, label = "Y", msg = msg Y }
        ]


resetButton : { label : String, msg : Msg } -> Html Msg
resetButton { label, msg } =
    div []
        [ button
            [ Html.Events.onClick msg ]
            [ text label ]
        ]
