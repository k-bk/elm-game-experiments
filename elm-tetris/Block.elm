module Block exposing (..)

import Basics exposing (..)
import Color exposing (Color)
import Collage exposing (..)
import Element exposing (..)
import List


type alias Block =
    { color : Color }


size : Float
size =
    25


toForm : Block -> Form
toForm block =
    let
        shape =
            square size

        border =
            outlined (solid Color.black) shape
    in
        group
            [ filled block.color shape
            , border
            ]


main =
    Block Color.blue
        |> toForm
        |> List.singleton
        |> Collage.collage 400 400
        |> Element.toHtml
