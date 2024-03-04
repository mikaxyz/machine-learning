module Main exposing (main)

import Commands.Train as Train
import Json.Decode as JD


type Msg
    = TrainMsg Train.Msg


type alias State =
    Result JD.Error Model


type alias Model =
    { command : CommandState
    }


type CommandState
    = TrainCommandState Train.Model


type Command
    = TrainCommand


type alias Flags =
    { command : Command
    }


flagsDecoder : JD.Decoder Flags
flagsDecoder =
    JD.map Flags
        (JD.field "command"
            (JD.string
                |> JD.andThen
                    (\x ->
                        case x of
                            "train" ->
                                JD.succeed TrainCommand

                            _ ->
                                JD.fail <| "Unknown command: " ++ x
                    )
            )
        )


main : Program JD.Value State Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


init : JD.Value -> ( State, Cmd Msg )
init flagsValue =
    case JD.decodeValue flagsDecoder flagsValue of
        Ok flags ->
            case flags.command of
                TrainCommand ->
                    Train.init flagsValue
                        |> Tuple.mapFirst
                            (Result.map
                                (\model ->
                                    { command = TrainCommandState model }
                                )
                            )
                        |> Tuple.mapSecond (Cmd.map TrainMsg)

        Err error ->
            let
                _ =
                    Debug.log "Failed to decode flags" (JD.errorToString error)
            in
            ( Err error, Cmd.none )


subscriptions : State -> Sub Msg
subscriptions modelResult =
    case modelResult of
        Ok model ->
            case model.command of
                TrainCommandState x ->
                    Train.subscriptions x |> Sub.map TrainMsg

        Err _ ->
            Sub.none


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case state of
        Ok model ->
            update_ msg model |> Tuple.mapFirst Ok

        Err _ ->
            ( state, Cmd.none )


update_ : Msg -> Model -> ( Model, Cmd Msg )
update_ msg model =
    case msg of
        TrainMsg msg_ ->
            case model.command of
                TrainCommandState commandStata ->
                    Train.update msg_ commandStata
                        |> Tuple.mapBoth
                            (\x -> { model | command = TrainCommandState x })
                            (Cmd.map TrainMsg)
