module App.NeuralNetworks.View.Dragon exposing (Dragon, Msg, currentVec2, init, subscriptions, update)

import Browser.Events
import Json.Decode as JD
import Math.Vector2 as Vec2 exposing (Vec2)
import Task


currentVec2 : Dragon -> Vec2
currentVec2 (Dragon dragon) =
    dragon.drag
        |> Maybe.map
            (\{ from, to } ->
                from
                    |> Vec2.sub to
                    |> Vec2.add dragon.current
            )
        |> Maybe.withDefault dragon.current


type Dragon
    = Dragon
        { drag : Maybe Drag
        , current : Vec2
        }


type alias Drag =
    { from : Vec2
    , to : Vec2
    }


type Msg
    = DragStart Vec2
    | DragUpdate Vec2
    | DragEnd Vec2


init : Dragon
init =
    Dragon
        { drag = Nothing
        , current = Vec2.vec2 0 0
        }


update : { tagger : Msg -> msg, onDragUpdate : Vec2 -> msg } -> Msg -> Dragon -> ( Dragon, Cmd msg )
update { onDragUpdate } msg (Dragon dragon) =
    case msg of
        DragStart position ->
            ( Dragon { dragon | drag = Just { from = position, to = position } }
            , Cmd.none
            )

        DragUpdate position ->
            case dragon.drag of
                Just drag ->
                    ( Dragon { dragon | drag = Just { drag | to = position } }
                    , Task.perform (\_ -> onDragUpdate (Vec2.sub drag.to position)) (Task.succeed ())
                    )

                Nothing ->
                    ( Dragon dragon, Cmd.none )

        DragEnd position ->
            ( Dragon
                { dragon
                    | drag = Nothing
                    , current = dragon.current |> Vec2.add position
                }
            , Cmd.none
            )


subscriptions : Dragon -> Sub Msg
subscriptions (Dragon dragon) =
    case dragon.drag of
        Just drag ->
            Sub.batch
                [ Browser.Events.onMouseUp (vectorDecoder |> JD.map (\end -> DragEnd (drag.from |> Vec2.sub end)))
                , Browser.Events.onMouseMove (vectorDecoder |> JD.map DragUpdate)
                ]

        Nothing ->
            Browser.Events.onMouseDown (vectorDecoder |> JD.map DragStart)


vectorDecoder : JD.Decoder Vec2
vectorDecoder =
    JD.map2 Vec2.vec2
        (JD.field "x" JD.float)
        (JD.field "y" JD.float)
