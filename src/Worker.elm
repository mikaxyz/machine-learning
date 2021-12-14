module Worker exposing (main)

import Process
import Random
import Task
import Worker.Task
import XYZMika.ML.NeuralNetwork as NeuralNetwork
    exposing
        ( NeuralNetwork
        , TrainingData
        )


type Msg
    = HandleTask Worker.Task.Task
    | TrainNeuralNetwork Int NeuralNetwork
    | TrainWithTrainingDataSet Int NeuralNetwork (List TrainingData)



--{ neuralNetwork : NeuralNetwork
--, trainingDataSet : List TrainingData
--, trainingDataSetInitialSize : Int
--}


type Model
    = Model Flags


type alias Flags =
    { version : String }


main : Program Flags Model Msg
main =
    Platform.worker
        { init = \flags -> ( Model flags, Cmd.none )
        , update = update
        , subscriptions = \_ -> Worker.Task.onStart HandleTask
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleTask task ->
            let
                _ =
                    Debug.log "Worker:HandleTask" task

                cmd =
                    case task of
                        Worker.Task.UnknownTask ->
                            Cmd.none

                        Worker.Task.TrainNeuralNetwork steps neuralNetwork ->
                            --Task.perform (\_ -> TrainNeuralNetwork steps neuralNetwork) (Process.sleep 1000)
                            Task.perform
                                (\_ ->
                                    TrainWithTrainingDataSet
                                        steps
                                        neuralNetwork
                                        (randomTrainingDataSet steps)
                                )
                                (Process.sleep 300)
            in
            ( model
            , cmd
            )

        TrainNeuralNetwork steps neuralNetwork ->
            let
                _ =
                    Debug.log "Worker:TrainNeuralNetwork" steps

                trained =
                    randomTrainingDataSet steps
                        |> List.foldl
                            (\trainingData nn ->
                                NeuralNetwork.train trainingData nn
                            )
                            neuralNetwork
            in
            ( model
            , Worker.Task.complete (Worker.Task.TrainedNeuralNetwork trained)
            )

        TrainWithTrainingDataSet trainingDataSetInitialSize neuralNetwork trainingDataSet ->
            case trainingDataSet of
                trainingData :: rest ->
                    let
                        nn : NeuralNetwork
                        nn =
                            NeuralNetwork.train trainingData neuralNetwork

                        complete : Float
                        complete =
                            1 - (toFloat (List.length rest) / toFloat trainingDataSetInitialSize)

                        delay =
                            case rest of
                                [] ->
                                    500

                                _ ->
                                    50
                    in
                    ( model
                    , Cmd.batch
                        [ Task.perform (\_ -> TrainWithTrainingDataSet trainingDataSetInitialSize nn rest) (Process.sleep delay)
                        , Worker.Task.progress (Worker.Task.Progress 1 complete)
                        ]
                    )

                [] ->
                    ( model
                    , Worker.Task.complete (Worker.Task.TrainedNeuralNetwork neuralNetwork)
                    )


randomTrainingDataSet : Int -> List TrainingData
randomTrainingDataSet size =
    Random.step
        (Random.list size (Random.int 0 3))
        (Random.initialSeed 666)
        |> Tuple.first
        |> List.map
            (\i ->
                case i of
                    0 ->
                        TrainingData [ 0, 0 ] [ 0 ]

                    1 ->
                        TrainingData [ 1, 1 ] [ 0 ]

                    2 ->
                        TrainingData [ 0, 1 ] [ 1 ]

                    _ ->
                        TrainingData [ 1, 0 ] [ 1 ]
            )
