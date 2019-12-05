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


splitBox : Direction -> Int -> Int -> Box -> ( Box, Box )
splitBox direction m n box =
    let
        ratio a b =
            toFloat a / toFloat (a + b)
    in
    case direction of
        Vertical ->
            ( { box | c = scale (ratio m n) box.c }, { box | a = add box.a (scale (ratio m n) box.c), c = scale (ratio n m) box.c } )

        Horizontal ->
            ( { box | b = scale (ratio m n) box.b }, { box | a = add box.a (scale (ratio m n) box.b), b = scale (ratio n m) box.b } )
