module Test.XYZMika.ML.NeuralNetwork exposing (suite)

import Expect exposing (Expectation)
import Random
import Test exposing (..)
import XYZMika.ML.Matrix as Matrix exposing (Matrix)
import XYZMika.ML.NeuralNetwork as NeuralNetwork exposing (NeuralNetwork, TrainingData)


suite : Test
suite =
    Test.concat
        [ test "outputs data from inputs" <|
            \_ ->
                let
                    neuralNetwork : NeuralNetwork
                    neuralNetwork =
                        NeuralNetwork.create
                            { inputs = 3
                            , outputs = 1
                            , activationFunction = NeuralNetwork.sigmoid
                            , randomSeed = Random.initialSeed 42
                            }

                    output =
                        neuralNetwork
                            |> NeuralNetwork.predict { inputs = [ 0, 1, 2 ] }
                            |> Matrix.toList
                in
                Expect.equal [ [ 0.31407280243810826 ] ] output
        , test "allows adding layers" <|
            \_ ->
                let
                    neuralNetwork : NeuralNetwork
                    neuralNetwork =
                        NeuralNetwork.create
                            { inputs = 2
                            , outputs = 1
                            , activationFunction = NeuralNetwork.sigmoid
                            , randomSeed = Random.initialSeed 42
                            }
                in
                Expect.equal
                    [ 1, 2, 3 ]
                    [ neuralNetwork |> NeuralNetwork.numberOfLayers
                    , neuralNetwork
                        |> NeuralNetwork.addLayer 2
                        |> NeuralNetwork.numberOfLayers
                    , neuralNetwork
                        |> NeuralNetwork.addLayer 2
                        |> NeuralNetwork.addLayer 2
                        |> NeuralNetwork.numberOfLayers
                    ]
        , test "has correct number of outputs" <|
            \_ ->
                let
                    predictWith : { inputs : Int, outputs : Int } -> Int
                    predictWith { inputs, outputs } =
                        NeuralNetwork.create
                            { inputs = inputs
                            , outputs = outputs
                            , activationFunction = NeuralNetwork.sigmoid
                            , randomSeed = Random.initialSeed 42
                            }
                            |> NeuralNetwork.predict { inputs = List.repeat inputs 0.5 }
                            |> Matrix.toList
                            |> List.concat
                            |> List.length

                    result : List Int
                    result =
                        [ predictWith { inputs = 1, outputs = 3 }
                        , predictWith { inputs = 2, outputs = 3 }
                        , predictWith { inputs = 3, outputs = 3 }
                        , predictWith { inputs = 4, outputs = 3 }
                        , predictWith { inputs = 12, outputs = 3 }
                        ]

                    expected =
                        List.repeat 5 3
                in
                Expect.equalLists expected result
        , test "has correct number of outputs with layers" <|
            \_ ->
                let
                    predictWith : { inputs : Int, outputs : Int } -> Int
                    predictWith { inputs, outputs } =
                        NeuralNetwork.create
                            { inputs = inputs
                            , outputs = outputs
                            , activationFunction = NeuralNetwork.sigmoid
                            , randomSeed = Random.initialSeed 42
                            }
                            |> NeuralNetwork.addLayer 3
                            |> NeuralNetwork.predict { inputs = List.repeat inputs 0.5 }
                            |> Matrix.toList
                            |> List.concat
                            |> List.length

                    result : List Int
                    result =
                        [ predictWith { inputs = 1, outputs = 3 }
                        , predictWith { inputs = 2, outputs = 3 }
                        , predictWith { inputs = 3, outputs = 3 }
                        , predictWith { inputs = 4, outputs = 3 }
                        , predictWith { inputs = 12, outputs = 3 }
                        ]

                    expected =
                        List.repeat 5 3
                in
                Expect.equalLists expected result
        , test "can learn" <|
            \_ ->
                let
                    neuralNetwork : NeuralNetwork
                    neuralNetwork =
                        NeuralNetwork.create
                            { inputs = 2
                            , outputs = 1
                            , activationFunction = NeuralNetwork.sigmoid
                            , randomSeed = Random.initialSeed 42
                            }

                    trained =
                        neuralNetwork
                            |> NeuralNetwork.train (TrainingData [ 1, 1 ] [ 0 ])
                            |> NeuralNetwork.predict { inputs = [ 1, 1 ] }

                    untrained =
                        neuralNetwork
                            |> NeuralNetwork.predict { inputs = [ 1, 1 ] }
                in
                Expect.notEqual untrained trained
        , test "sanity check..." <|
            \_ ->
                let
                    trainingData =
                        TrainingData [ 0, 1 ] [ 1, 1, 1 ]

                    neuralNetwork : NeuralNetwork
                    neuralNetwork =
                        NeuralNetwork.create
                            { inputs = List.length trainingData.inputs
                            , outputs = List.length trainingData.expected
                            , activationFunction = NeuralNetwork.sigmoid
                            , randomSeed = Random.initialSeed 42
                            }
                            --|> NeuralNetwork.addLayer 2
                            |> NeuralNetwork.train trainingData

                    out1 : List Float
                    out1 =
                        neuralNetwork
                            |> NeuralNetwork.predict { inputs = [ 1, 0.1 ] }
                            |> Matrix.toList
                            |> List.concat

                    out2 : List Float
                    out2 =
                        neuralNetwork
                            |> NeuralNetwork.predict { inputs = [ 1, 0.2 ] }
                            |> Matrix.toList
                            |> List.concat
                in
                Expect.all
                    [ \x -> Expect.equal 3 (List.length x)
                    , Expect.notEqual out2
                    ]
                    out1
        , test "can learn to be XOR gate" <|
            \_ ->
                let
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

                    neuralNetwork : NeuralNetwork
                    neuralNetwork =
                        NeuralNetwork.create
                            { inputs = 2
                            , outputs = 1
                            , activationFunction = NeuralNetwork.sigmoid
                            , randomSeed = Random.initialSeed 4223
                            }
                            |> NeuralNetwork.addLayer 3
                            |> trainIt

                    trainIt neuralNetwork_ =
                        randomTrainingDataSet 20000
                            |> List.foldl
                                (\trainingData nn ->
                                    NeuralNetwork.train trainingData nn
                                )
                                neuralNetwork_

                    neuralNetworkOutputSingleValue : Matrix -> Float
                    neuralNetworkOutputSingleValue x =
                        x
                            |> Matrix.toList
                            |> List.concat
                            |> List.head
                            |> Maybe.withDefault -1000

                    output inputs =
                        neuralNetwork
                            |> NeuralNetwork.predict { inputs = inputs }
                            |> neuralNetworkOutputSingleValue
                in
                Expect.all
                    [ \_ ->
                        output [ 0, 0 ]
                            |> Expect.atMost 0.05
                    , \_ ->
                        output [ 0, 1 ]
                            |> Expect.atLeast 0.95
                    , \_ ->
                        output [ 1, 0 ]
                            |> Expect.atLeast 0.95
                    , \_ ->
                        output [ 1, 1 ]
                            |> Expect.atMost 0.05
                    , \_ ->
                        output [ 0.8, 1 ]
                            |> Expect.atMost 0.05
                    ]
                    neuralNetwork
        ]
