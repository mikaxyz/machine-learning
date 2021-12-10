module Main exposing (main)

import Browser
import Browser.Events
import Html
import Math.Vector2 as Vec2 exposing (vec2)
import Math.Vector3 as Vec3
import Random
import Task


type Msg
    = Msg


type Model
    = Model


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( Model, Cmd.none )
        , view =
            \model ->
                { title = "Machine Learning"
                , body = [ Html.text "TODO..." ]
                }
        , update = update
        , subscriptions = \_ -> Sub.none
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg ->
            ( model
            , Cmd.none
            )
