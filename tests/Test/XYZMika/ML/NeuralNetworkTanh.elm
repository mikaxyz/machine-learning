module Test.XYZMika.ML.NeuralNetworkTanh exposing (suite)

import Expect exposing (Expectation)
import Random
import Test exposing (..)
import XYZMika.ML.ActivationFunction as ActivationFunction
import XYZMika.ML.NeuralNetwork as NeuralNetwork exposing (NeuralNetwork, TrainingData)


suite : Test
suite =
    Test.concat
        [ test "can learn to be XOR gate (tanh, centered)" <|
            \_ ->
                let
                    -- XOR over centered inputs/targets for tanh
                    baseSet : List TrainingData
                    baseSet =
                        [ TrainingData [ -1, -1 ] [ -1 ]
                        , TrainingData [ -1, 1 ] [ 1 ]
                        , TrainingData [ 1, -1 ] [ 1 ]
                        , TrainingData [ 1, 1 ] [ -1 ]
                        ]

                    trainingData : List TrainingData
                    trainingData =
                        List.repeat 20000 baseSet |> List.concat

                    neuralNetwork : NeuralNetwork
                    neuralNetwork =
                        NeuralNetwork.configure
                            { randomSeed = Random.initialSeed 42
                            , inputs = 2
                            , outputs = 1
                            }
                            |> NeuralNetwork.addLayer { neurons = 6 }
                            |> NeuralNetwork.addLayer { neurons = 4 }
                            |> NeuralNetwork.withActivationFunction ActivationFunction.Tanh
                            |> NeuralNetwork.withLearningRate 0.1
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
                        predict [ -1, -1 ]
                            |> Expect.atMost -0.9
                    , \_ ->
                        predict [ -1, 1 ]
                            |> Expect.atLeast 0.9
                    , \_ ->
                        predict [ 1, -1 ]
                            |> Expect.atLeast 0.9
                    , \_ ->
                        predict [ 1, 1 ]
                            |> Expect.atMost -0.9
                    ]
                    ()
        ]