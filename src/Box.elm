module Box exposing (..)

import Vector exposing (..)


type alias Box =
    { a : Vector
    , b : Vector
    , c : Vector
    }



-- Exercise 1


turnBox : Box -> Box
turnBox { a, b, c } =
    { a = add a b
    , b = c
    , c = neg b
    }



-- Excercise 2


flipBox : Box -> Box
flipBox { a, b, c } =
    { a = add a b
    , b = neg b
    , c = c
    }



-- Excercise 3


tossBox : Box -> Box
tossBox { a, b, c } =
    { a = add a (scale 0.5 (add b c))
    , b = scale 0.5 (add b c)
    , c = scale 0.5 (add c (neg b))
    }



-- Excercise 4


type Direction
    = Vertical
    | Horizontal


splitBox : Direction -> Box -> ( Box, Box )
splitBox direction box =
    case direction of
        Vertical ->
            ( { box | c = scale 0.5 box.c }, { box | a = add box.a (scale 0.5 box.c), c = scale 0.5 box.c } )

        Horizontal ->
            ( box, box )
