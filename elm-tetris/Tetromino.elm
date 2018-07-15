module Tetromino exposing (..)

import Basics exposing (..)
import Block exposing (Block)
import Color exposing (Color)
import Collage exposing (..)
import Element exposing (..)
import Html exposing (Html)
import List


type alias Location =
    ( Int, Int )


type alias Tetromino =
    { shape : List Location
    , block : Block
    , pivot : { c : Float, r : Float }
    , cols : Int
    , rows : Int
    }


toForm : Tetromino -> Form
toForm { shape, block } =
    let
        form =
            Block.toForm block

        translate ( col, row ) =
            move ( (toFloat col) * Block.size, (toFloat row) * Block.size ) form

        forms =
            List.map translate shape
    in
        group forms


t : Tetromino
t =
    { shape =
        [ ( -1, 0 )
        , ( 0, 0 )
        , ( 1, 0 )
        , ( 0, -1 )
        ]
    , block = Block Color.yellow
    , pivot = { c = 0.0, r = 0.0 }
    , cols = 3
    , rows = 2
    }


j : Tetromino
j =
    { shape =
        [ ( 0, 1 )
        , ( 0, 0 )
        , ( 0, -1 )
        , ( -1, -1 )
        ]
    , block = Block Color.lightBlue
    , pivot = { c = 0.0, r = 0.0 }
    , cols = 2
    , rows = 3
    }


l : Tetromino
l =
    { shape =
        [ ( 0, 1 )
        , ( 0, 0 )
        , ( 0, -1 )
        , ( 1, -1 )
        ]
    , block = Block Color.red
    , pivot = { c = 0.0, r = 0.0 }
    , cols = 2
    , rows = 3
    }


s : Tetromino
s =
    { shape =
        [ ( 1, 0 )
        , ( 0, 0 )
        , ( 0, -1 )
        , ( -1, -1 )
        ]
    , block = Block Color.green
    , pivot = { c = 0.0, r = 0.0 }
    , cols = 3
    , rows = 2
    }


z : Tetromino
z =
    { shape =
        [ ( -1, 0 )
        , ( 0, 0 )
        , ( 0, -1 )
        , ( 1, -1 )
        ]
    , block = Block Color.purple
    , pivot = { c = 0.0, r = 0.0 }
    , cols = 3
    , rows = 2
    }


i : Tetromino
i =
    { shape =
        [ ( 0, 1 )
        , ( 0, 0 )
        , ( 0, -1 )
        , ( 0, -2 )
        ]
    , block = Block Color.lightGreen
    , pivot = { c = 0.5, r = -0.5 }
    , cols = 1
    , rows = 4
    }


o : Tetromino
o =
    { shape =
        [ ( 0, 0 )
        , ( -1, 0 )
        , ( -1, -1 )
        , ( 0, -1 )
        ]
    , block = Block Color.orange
    , pivot = { c = -0.5, r = -0.5 }
    , cols = 2
    , rows = 2
    }


drawPivot : Tetromino -> Form
drawPivot { pivot } =
    circle 5
        |> filled Color.black
        |> move ( pivot.c * Block.size, pivot.r * Block.size )


rotateLocation : { c : Float, r : Float } -> Float -> Location -> Location
rotateLocation pivot angle ( col, row ) =
    let
        colOrigin =
            (toFloat col) - pivot.c

        rowOrigin =
            (toFloat row) - pivot.r

        ( c, s ) =
            ( cos (angle), sin (angle) )

        colRotated =
            colOrigin * c - rowOrigin * s

        rowRotated =
            colOrigin * s + rowOrigin * c
    in
        ( round <| colRotated + pivot.c, round <| rowRotated + pivot.r )


rotate : Tetromino -> Tetromino
rotate tetromino =
    let
        rotateHelper =
            rotateLocation tetromino.pivot (degrees 90)

        newShape =
            List.map rotateHelper tetromino.shape
    in
        { tetromino
            | shape = newShape
            , cols = tetromino.rows
            , rows = tetromino.cols
        }


shift : ( Int, Int ) -> Tetromino -> Tetromino
shift ( cols, rows ) tetromino =
    let
        shiftHelper ( col, row ) =
            ( col + cols, row + rows )

        newPivot =
            { c = tetromino.pivot.c + toFloat cols
            , r = tetromino.pivot.r + toFloat rows
            }
    in
        { tetromino
            | shape = List.map shiftHelper tetromino.shape
            , pivot = newPivot
        }
