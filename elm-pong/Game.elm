module Game exposing (..)

import Color exposing (Color)
import Element exposing (Element, toHtml)
import Html exposing (Html, text, div)
import Html.Attributes exposing (style)
import AnimationFrame
import Time exposing (Time)
import Key exposing (..)
import Keyboard exposing (KeyCode)


-- MODEL


type alias Model =
    { position : Float
    , velocity : Float
    , shotsFired : Int
    }


model : Model
model =
    { position = 0
    , velocity = 0
    , shotsFired = 0
    }


init : ( Model, Cmd Msg )
init =
    ( model, Cmd.none )



-- UPDATE


type Msg
    = TimeUpdate Time
    | KeyDown KeyCode
    | KeyUp KeyCode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimeUpdate dt ->
            ( applyPhysics dt model, Cmd.none )

        KeyDown keyCode ->
            ( keyDown keyCode model, Cmd.none )

        KeyUp keyCode ->
            ( keyUp keyCode model, Cmd.none )


keyDown : KeyCode -> Model -> Model
keyDown keyCode model =
    case Key.fromCode keyCode of
        Space ->
            incrementShotsFired model

        ArrowLeft ->
            updateVelocity (-1.0) model

        ArrowRight ->
            updateVelocity (1.0) model

        _ ->
            model


keyUp : KeyCode -> Model -> Model
keyUp keyCode model =
    case Key.fromCode keyCode of
        ArrowLeft ->
            updateVelocity 0 model

        ArrowRight ->
            updateVelocity 0 model

        _ ->
            model


applyPhysics : Time -> Model -> Model
applyPhysics dt model =
    let
        newPos =
            clamp 0 300 (model.position + (model.velocity * dt))
    in
        { model | position = newPos }


updateVelocity : Float -> Model -> Model
updateVelocity newVelocity model =
    { model | velocity = newVelocity }


incrementShotsFired : Model -> Model
incrementShotsFired model =
    { model | shotsFired = model.shotsFired + 1 }



-- VIEW


view : Model -> Html msg
view model =
    let
        ship =
            Element.toHtml spaceship
    in
        div
            [ style
                [ ( "width", "640px" )
                , ( "height", "480px" )
                , ( "posiiton", "absolute" )
                ]
            ]
            [ ship ]


spaceship : Element
spaceship =
    Element.image 128 128 "assets/graphics/player_ship.png"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs TimeUpdate
        , Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        ]



-- MAIN


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
