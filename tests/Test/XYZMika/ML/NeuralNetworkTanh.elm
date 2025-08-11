module Test.XYZMika.ML.NeuralNetworkTanh exposing (suite)

import Expect exposing (Expectation)
import Random
import Test exposing (..)
import XYZMika.ML.ActivationFunction as ActivationFunction
import XYZMika.ML.NeuralNetwork as NeuralNetwork exposing (NeuralNetwork, TrainingData)


suite : Test
suite =
    Test.concat
        [ test "can learn to be XOR gate (tanh 0..1)" <|
            \_ ->
                let
                    baseSet : List TrainingData
                    baseSet =
                        [ TrainingData [ 0, 0 ] [ 0 ]
                        , TrainingData [ 0, 1 ] [ 1 ]
                        , TrainingData [ 1, 0 ] [ 1 ]
                        , TrainingData [ 1, 1 ] [ 0 ]
                        ]

                    trainingData : List TrainingData
                    trainingData =
                        List.repeat 10000 baseSet |> List.concat

                    neuralNetwork : NeuralNetwork
                    neuralNetwork =
                        NeuralNetwork.configure
                            { randomSeed = Random.initialSeed 42
                            , inputs = 2
                            , outputs = 1
                            }
                            |> NeuralNetwork.addLayer { neurons = 2 }
                            |> NeuralNetwork.withActivationFunction ActivationFunction.Tanh
                            |> NeuralNetwork.withLearningRate 0.3
                            |> NeuralNetwork.create
                            |> train

                    train : NeuralNetwork -> NeuralNetwork
                    train nn =
                        trainingData
                            |> List.foldl (\td acc -> NeuralNetwork.train td acc) nn

                    predict : List Float -> Float
                    predict inputs =
                        neuralNetwork
                            |> NeuralNetwork.predict { inputs = inputs }
                            |> List.head
                            |> Maybe.withDefault -1000
                in
                Expect.all
                    [ \_ ->
                        predict [ 0, 0 ]
                            |> Expect.atMost 0.1
                    , \_ ->
                        predict [ 0, 1 ]
                            |> Expect.atLeast 0.9
                    , \_ ->
                        predict [ 1, 0 ]
                            |> Expect.atLeast 0.9
                    , \_ ->
                        predict [ 1, 1 ]
                            |> Expect.atMost 0.1
                    ]
                    ()
        ]
