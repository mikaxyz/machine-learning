port module Main exposing (main)

import ConcurrentTask exposing (ConcurrentTask)
import Json.Decode as JD
import Json.Encode as JE
import Random
import Task
import XYZMika.ML.ActivationFunction as ActivationFunction
import XYZMika.ML.NeuralNetwork as NeuralNetwork exposing (NeuralNetwork)


port send : JD.Value -> Cmd msg


port receive : (JD.Value -> msg) -> Sub msg


type TaskError
    = DecodeError JD.Error
    | IOError


type TaskSuccess
    = TrainingData (List ImageData)
    | ModelSaved String


type Msg
    = OnProgress ( ConcurrentTask.Pool Msg TaskError TaskSuccess, Cmd Msg )
    | OnComplete (ConcurrentTask.Response TaskError TaskSuccess)
    | Train ImageData
    | TrainWithData


type alias Model =
    { tasks : ConcurrentTask.Pool Msg TaskError TaskSuccess
    , neuralNetwork : NeuralNetwork
    , trainingData : List ImageData
    }


type alias Flags =
    { fileName : String }


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init { fileName } =
    let
        ( tasks, cmd ) =
            ConcurrentTask.attempt
                { send = send
                , pool = ConcurrentTask.pool
                , onComplete = OnComplete
                }
                (getFile fileName fileDecoder |> ConcurrentTask.map TrainingData)
    in
    ( { tasks = tasks
      , neuralNetwork =
            NeuralNetwork.configure
                { randomSeed = Random.initialSeed 42
                , inputs = 784
                , outputs = 10
                }
                |> NeuralNetwork.withActivationFunction ActivationFunction.Tanh
                |> NeuralNetwork.addLayer { neurons = 10 }
                |> NeuralNetwork.create
      , trainingData = []
      }
    , cmd
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    ConcurrentTask.onProgress
        { send = send
        , receive = receive
        , onProgress = OnProgress
        }
        model.tasks


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnComplete (ConcurrentTask.Success (TrainingData trainingData)) ->
            let
                _ =
                    Debug.log "TrainWithData" (List.length trainingData)
            in
            ( { model | trainingData = trainingData }
            , Task.succeed () |> Task.perform (\_ -> TrainWithData)
            )

        OnComplete (ConcurrentTask.Success (ModelSaved path)) ->
            let
                _ =
                    Debug.log "Saved model: " path
            in
            ( model
            , Cmd.none
            )

        OnComplete error ->
            let
                _ =
                    Debug.log "ERROR" error
            in
            ( model, Cmd.none )

        OnProgress ( tasks, cmd ) ->
            ( { model | tasks = tasks }, cmd )

        TrainWithData ->
            case model.trainingData of
                current :: rest ->
                    ( { model | trainingData = rest }
                    , Task.succeed current |> Task.perform Train
                    )

                [] ->
                    let
                        --_ =
                        --    Debug.log "TRAIN" imageData.label
                        ( tasks, cmd ) =
                            ConcurrentTask.attempt
                                { send = send
                                , pool = ConcurrentTask.pool
                                , onComplete = OnComplete
                                }
                                (saveModel model.neuralNetwork |> ConcurrentTask.map ModelSaved)
                    in
                    ( { model | tasks = tasks }
                    , cmd
                    )

        Train imageData ->
            --let
            --    _ =
            --        Debug.log "TRAIN" imageData.label
            --in
            ( { model
                | neuralNetwork =
                    model.neuralNetwork
                        |> NeuralNetwork.train
                            { inputs = imageData.values |> List.map (\x -> toFloat x / 255)
                            , expected = labelToOutput10 imageData.label
                            }
              }
            , Task.succeed () |> Task.perform (\_ -> TrainWithData)
            )


labelToOutput10 : Int -> List number
labelToOutput10 label =
    case label of
        0 ->
            [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 ]

        1 ->
            [ 0, 0, 0, 0, 0, 0, 0, 0, 1, 0 ]

        2 ->
            [ 0, 0, 0, 0, 0, 0, 0, 1, 0, 0 ]

        3 ->
            [ 0, 0, 0, 0, 0, 0, 1, 0, 0, 0 ]

        4 ->
            [ 0, 0, 0, 0, 0, 1, 0, 0, 0, 0 ]

        5 ->
            [ 0, 0, 0, 0, 1, 0, 0, 0, 0, 0 ]

        6 ->
            [ 0, 0, 0, 1, 0, 0, 0, 0, 0, 0 ]

        7 ->
            [ 0, 0, 1, 0, 0, 0, 0, 0, 0, 0 ]

        8 ->
            [ 0, 1, 0, 0, 0, 0, 0, 0, 0, 0 ]

        _ ->
            [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]


type alias ImageData =
    { label : Int
    , values : List Int
    }


imageDataFromString : String -> Result Int ImageData
imageDataFromString x =
    case String.split "," x |> List.map String.toInt of
        (Just label) :: data ->
            { label = label
            , values = data |> List.filterMap identity
            }
                |> Ok

        _ ->
            Err 0


imageDataDecoder : JD.Decoder ImageData
imageDataDecoder =
    JD.string
        |> JD.andThen
            (\x ->
                case imageDataFromString x of
                    Ok imageData ->
                        JD.succeed imageData

                    Err _ ->
                        JD.fail "Could not decode image data"
            )


fileDecoder : JD.Decoder (List ImageData)
fileDecoder =
    JD.string
        |> JD.andThen
            (\fileContent ->
                let
                    results : { errors : List Int, images : List ImageData }
                    results =
                        String.lines fileContent
                            |> List.map imageDataFromString
                            |> List.foldl
                                (\x a ->
                                    case x of
                                        Err err ->
                                            { a | errors = err :: a.errors }

                                        Ok image ->
                                            { a | images = image :: a.images }
                                )
                                { errors = [], images = [] }
                in
                case ( results.errors, results.images ) of
                    ( [], [] ) ->
                        JD.fail "No data"

                    ( errors, [] ) ->
                        JD.fail ("Failed to decode " ++ (List.length errors |> String.fromInt) ++ " rows")

                    ( _, images ) ->
                        JD.succeed images
            )


decodeErrors : JD.Decoder TaskError
decodeErrors =
    JD.string
        |> JD.andThen
            (\reason ->
                case reason of
                    "IOError" ->
                        JD.succeed IOError

                    _ ->
                        JD.fail ("Unrecognized ReadError " ++ reason)
            )


getFile : String -> JD.Decoder (List ImageData) -> ConcurrentTask TaskError (List ImageData)
getFile fileName decoder =
    ConcurrentTask.define
        { function = "cli:readFile"
        , expect = ConcurrentTask.expectString
        , errors = ConcurrentTask.expectErrors decodeErrors
        , args = JE.object [ ( "fileName", JE.string fileName ) ]
        }
        |> ConcurrentTask.map (JD.decodeString decoder >> Result.mapError DecodeError)
        |> ConcurrentTask.andThen ConcurrentTask.fromResult


saveModel : NeuralNetwork -> ConcurrentTask TaskError String
saveModel neuralNetwork =
    ConcurrentTask.define
        { function = "cli:saveModel"
        , expect = ConcurrentTask.expectString
        , errors = ConcurrentTask.expectErrors decodeErrors
        , args = NeuralNetwork.encode neuralNetwork
        }
