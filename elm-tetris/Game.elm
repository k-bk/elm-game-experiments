module Game exposing (..)

import AnimationFrame
import Basics exposing (..)
import Board exposing (Board)
import Collage exposing (collage)
import Element exposing (..)
import Html exposing (Html)
import Key exposing (..)
import Keyboard exposing (KeyCode)
import Tetromino exposing (..)
import Time exposing (Time)


-- MODEL


type alias Model =
    { falling : Tetromino
    , board : Board
    , shiftRemainder : Time
    , shiftDelay : Time
    }


model : Model
model =
    { falling = Tetromino.shift startingShift t
    , board = Board.new []
    , shiftRemainder = 0
    , shiftDelay = Time.second
    }


init : ( Model, Cmd Msg )
init =
    ( model, Cmd.none )


startingShift : ( Int, Int )
startingShift =
    ( 5, 20 )



-- UPDATE


type Msg
    = KeyDown KeyCode
    | TimeUpdate Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        useModelIfValid =
            useIfValid model
    in
        case msg of
            KeyDown keyCode ->
                ( useModelIfValid <| updateFalling (keysToInput keyCode) model, Cmd.none )

            TimeUpdate dt ->
                ( useModelIfValid <| checkTick { model | shiftRemainder = model.shiftRemainder + dt }, Cmd.none )


updateFalling : Input -> Model -> Model
updateFalling input model =
    case input of
        Rotate ->
            let rotated = 
            { model | falling = rotate model.falling }
            nextModel = useIfValid' rotated
            nextModel' =
                if nextModel = model then wallKick state rotated else model
            in
                nextModel'

        Shift amount ->
            { model | falling = shift amount model.falling }


checkTick : Model -> Model
checkTick model =
    if model.shiftRemainder < model.shiftDelay then
        model
    else
        { model
            | falling = shift ( 0, -1 ) model.falling
            , shiftRemainder = model.shiftRemainder - model.shiftDelay
        }


useIfValid : Model -> Model -> Model
useIfValid current new =
    if Board.isValid new.falling new.board then
        new
    else
        current


tryKicks : List ( Int, Int ) -> Model -> Model -> Model
tryKicks shifts current nextModel =
    case shifts of
        [] ->
            current

        s :: rest ->
            let
                shifted =
                    Tetromino.shift s nextModel.falling
            in
                if Board.isValid shifted nextModel.board then
                    { nextModel | falling = shifted }
                else
                    tryKicks rest current nextModel


wallKick : Model -> Model -> Model
wallKick current nextModel =
    let
        range =
            nextModel.falling.cols // 2

        shifts =
            [1..range] |> List.concatMap (\n -> [ ( 0, n ), ( 0, -n ) ])
    in
        tryKicks shifts current nextModel


type Input
    = Rotate
    | Shift ( Int, Int )


keysToInput : KeyCode -> Input
keysToInput keyCode =
    case fromCode keyCode of
        ArrowUp ->
            Rotate

        ArrowLeft ->
            Shift ( -1, 0 )

        ArrowRight ->
            Shift ( 1, 0 )

        ArrowDown ->
            Shift ( 0, -1 )

        Unknown ->
            Shift ( 0, 0 )



-- VIEW


view : Model -> Html Msg
view model =
    let
        screenWidth =
            800

        screenHeight =
            600

        boardForm =
            Board.addTetromino model.falling model.board |> Board.toForm
    in
        [ boardForm ]
            |> collage screenWidth screenHeight
            |> toHtml



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs KeyDown
        , AnimationFrame.diffs TimeUpdate
        ]



-- MAIN


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
