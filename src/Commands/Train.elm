module Commands.Train exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    )

import ConcurrentTask exposing (ConcurrentTask)
import Json.Decode as JD
import Port
import Random
import Task
import Tasks exposing (TaskError, TaskSuccess(..))
import XYZMika.Debug
import XYZMika.ML.ActivationFunction as ActivationFunction exposing (ActivationFunction(..))
import XYZMika.ML.NeuralNetwork as NeuralNetwork exposing (NeuralNetwork)


type alias Flags =
    { fileName : String
    , learningRate : Float
    , activationFunction : ActivationFunction
    , layers : List Int
    , seed : Int
    }


activationFunctionDecoder : JD.Decoder ActivationFunction
activationFunctionDecoder =
    JD.string
        |> JD.andThen
            (\id ->
                case id of
                    "sigmoid" ->
                        JD.succeed ActivationFunction.Sigmoid

                    "tanh" ->
                        JD.succeed ActivationFunction.Tanh

                    _ ->
                        JD.fail ("unknown value for ActivationFunction: " ++ id)
            )


flagsDecoder : JD.Decoder Flags
flagsDecoder =
    JD.map5 Flags
        (JD.field "fileName" JD.string)
        (JD.field "learningRate" JD.float)
        (JD.field "activationFunction" activationFunctionDecoder)
        (JD.field "layers" (JD.list JD.int))
        (JD.field "seed" JD.int)


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


type alias ImageData =
    { label : Int
    , values : List Int
    }


init : JD.Value -> ( Result JD.Error Model, Cmd Msg )
init flagsValue =
    case JD.decodeValue flagsDecoder flagsValue of
        Ok flags ->
            initWithFlags flags |> Tuple.mapFirst Ok

        Err error ->
            let
                _ =
                    XYZMika.Debug.log "Failed to decode flags for train command" (JD.errorToString error)
            in
            ( Err error, Cmd.none )


initWithFlags : Flags -> ( Model, Cmd Msg )
initWithFlags flags =
    let
        ( tasks, cmd ) =
            ConcurrentTask.attempt
                { send = Port.send
                , pool = ConcurrentTask.pool
                , onComplete = OnComplete
                }
                (Tasks.readMnistCsv flags.fileName |> ConcurrentTask.map TrainingData)

        addLayers : List Int -> NeuralNetwork.Configuration -> NeuralNetwork.Configuration
        addLayers layers neuralNetwork =
            layers
                |> List.reverse
                |> List.foldl (\neurons -> NeuralNetwork.addLayer { neurons = neurons }) neuralNetwork
    in
    ( { tasks = tasks
      , neuralNetwork =
            NeuralNetwork.configure
                { randomSeed = Random.initialSeed flags.seed
                , inputs = 784
                , outputs = 10
                }
                |> NeuralNetwork.withActivationFunction flags.activationFunction
                |> NeuralNetwork.withLearningRate flags.learningRate
                |> addLayers flags.layers
                |> NeuralNetwork.create
      , trainingData = []
      }
    , cmd
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    ConcurrentTask.onProgress
        { send = Port.send
        , receive = Port.receive
        , onProgress = OnProgress
        }
        model.tasks


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnComplete (ConcurrentTask.Success (TrainingData trainingData)) ->
            let
                _ =
                    XYZMika.Debug.log "TrainWithData" (List.length trainingData)
            in
            ( { model | trainingData = trainingData }
            , Task.succeed () |> Task.perform (\_ -> TrainWithData)
            )

        OnComplete (ConcurrentTask.Success (ModelSaved path)) ->
            let
                _ =
                    XYZMika.Debug.log "Saved model: " path
            in
            ( model
            , Cmd.none
            )

        OnComplete error ->
            let
                _ =
                    XYZMika.Debug.log "ERROR" error
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
                        --    XYZMika.Debug.log "TRAIN" imageData.label
                        ( tasks, cmd ) =
                            ConcurrentTask.attempt
                                { send = Port.send
                                , pool = ConcurrentTask.pool
                                , onComplete = OnComplete
                                }
                                (Tasks.saveModel model.neuralNetwork |> ConcurrentTask.map ModelSaved)
                    in
                    ( { model | tasks = tasks }
                    , cmd
                    )

        Train imageData ->
            --let
            --    _ =
            --        XYZMika.Debug.log "TRAIN" imageData.label
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
