module Board exposing (..)

import Block exposing (Block)
import Collage exposing (..)
import Color
import Dict exposing (Dict)
import Element exposing (down, flow, show)
import Tetromino exposing (Tetromino, Location)


type alias Board =
    Dict Location Block


new : List ( Location, Block ) -> Board
new =
    Dict.fromList


cols : Int
cols =
    10


rows : Int
rows =
    20


background : Form
background =
    let
        shape =
            rect ((toFloat cols) * Block.size) ((toFloat rows) * Block.size)

        border =
            outlined (solid Color.black) shape
    in
        group [ border, filled Color.black shape ]


addBlock : Location -> Block -> Form -> Form
addBlock ( col, row ) block form =
    let
        offSetX =
            -(toFloat cols - 1) / 2 * Block.size

        offSetY =
            -(toFloat rows - 1) / 2 * Block.size

        x =
            (toFloat col) * Block.size

        y =
            (toFloat row) * Block.size

        blockForm =
            Block.toForm block |> move ( offSetX + x, offSetY + y )
    in
        group [ form, blockForm ]


toForm : Board -> Form
toForm board =
    Dict.foldr addBlock background board


testForm : Form
testForm =
    background
        |> addBlock ( 0, 0 ) (Block Color.blue)
        |> addBlock ( 0, 1 ) (Block Color.yellow)
        |> addBlock ( 1, 0 ) (Block Color.red)


testBoard : Board
testBoard =
    new
        [ ( ( 0, 0 ), Block Color.blue )
        , ( ( 0, 1 ), Block Color.yellow )
        , ( ( 1, 0 ), Block Color.red )
        , ( ( 1, 1 ), Block Color.green )
        ]


cumulativeSum : List Int -> List Int
cumulativeSum =
    List.scanl (+) 0


iota : Int -> List Int
iota n =
    List.repeat (n - 1) 1 |> cumulativeSum


fillRow : Int -> Block -> Board -> Board
fillRow row block board =
    let
        columns =
            iota cols

        rows =
            List.repeat cols row

        locations =
            List.map2 (,) columns rows

        blocks =
            List.repeat cols block

        filledRow =
            List.map2 (,) locations blocks |> new
    in
        Dict.union filledRow board


checkRow : Int -> Board -> Bool
checkRow row board =
    let
        blocks =
            Dict.filter (\( _, r ) _ -> r == row) board
    in
        Dict.size blocks == cols


clearRow : Int -> Board -> Board
clearRow row board =
    let
        shift ( c, r ) block newBoard =
            if (r < row) then
                (Dict.insert ( c, r ) block newBoard)
            else if (r > row) then
                (Dict.insert ( c, r - 1 ) block newBoard)
            else
                newBoard
    in
        Dict.foldr shift Dict.empty board


clearLines : Board -> ( Int, Board )
clearLines =
    let
        clearProcess currentRow cleared board =
            if (currentRow >= rows) then
                ( cleared, board )
            else if (checkRow currentRow board) then
                clearProcess currentRow (cleared + 1) (clearRow currentRow board)
            else
                clearProcess (currentRow + 1) cleared board
    in
        clearProcess 0 0


addTetromino : Tetromino -> Board -> Board
addTetromino { shape, block } board =
    let
        asBoard =
            List.map2 (,) shape (List.repeat 4 block) |> new
    in
        Dict.union asBoard board


inBounds : Tetromino -> Bool
inBounds { shape } =
    let
        checkLocation ( c, r ) =
            r >= 0 && c >= 0 && c < cols
    in
        List.all checkLocation shape


isIntersecting : Tetromino -> Board -> Bool
isIntersecting { shape } board =
    let
        checkLocation location =
            Dict.member location board
    in
        List.any checkLocation shape


isValid : Tetromino -> Board -> Bool
isValid tetromino board =
    (inBounds tetromino) && not (isIntersecting tetromino board)


tetromino =
    Tetromino.shift ( 5, 5 ) Tetromino.j


test =
    new []
        |> fillRow 0 (Block Color.red)
        |> fillRow 1 (Block Color.yellow)
        |> fillRow 2 (Block Color.blue)
        |> Dict.remove ( 0, 1 )
        |> clearLines
        |> Tuple.second


main =
    flow down
        [ collage 600 600 [ toForm (addTetromino tetromino test) ]
        , show <| isValid tetromino test
        ]
        |> Element.toHtml
