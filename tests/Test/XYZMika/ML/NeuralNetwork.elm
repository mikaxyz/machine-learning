module Test.XYZMika.ML.NeuralNetwork exposing (suite)

import Expect exposing (Expectation)
import Json.Decode as JD
import Random
import Test exposing (..)
import XYZMika.ML.ActivationFunction as ActivationFunction
import XYZMika.ML.NeuralNetwork as NeuralNetwork exposing (NeuralNetwork, TrainingData)


suite : Test
suite =
    Test.concat
        [ test "outputs data from inputs" <|
            \_ ->
                let
                    neuralNetwork : NeuralNetwork
                    neuralNetwork =
                        NeuralNetwork.configure
                            { randomSeed = Random.initialSeed 42
                            , inputs = 3
                            , outputs = 1
                            }
                            |> NeuralNetwork.create

                    output : List Float
                    output =
                        neuralNetwork
                            |> NeuralNetwork.predict { inputs = [ 0, 1, 2 ] }
                in
                Expect.equal [ 0.31407280243810826 ] output
        , test "uses activation functions" <|
            \_ ->
                let
                    sigmoidOutput : List Float
                    sigmoidOutput =
                        NeuralNetwork.configure
                            { randomSeed = Random.initialSeed 42
                            , inputs = 3
                            , outputs = 1
                            }
                            |> NeuralNetwork.create
                            |> NeuralNetwork.predict { inputs = [ 1, 2, 3 ] }

                    tanhOutput : List Float
                    tanhOutput =
                        NeuralNetwork.configure
                            { randomSeed = Random.initialSeed 42
                            , inputs = 3
                            , outputs = 1
                            }
                            |> NeuralNetwork.withActivationFunction
                                ActivationFunction.Tanh
                            |> NeuralNetwork.create
                            |> NeuralNetwork.predict { inputs = [ 1, 2, 3 ] }
                in
                Expect.notEqual sigmoidOutput tanhOutput
        , test "uses learning rate" <|
            \_ ->
                let
                    neuralNetworkConfig =
                        NeuralNetwork.configure
                            { randomSeed = Random.initialSeed 42
                            , inputs = 3
                            , outputs = 1
                            }

                    output1 : List Float
                    output1 =
                        neuralNetworkConfig
                            |> NeuralNetwork.withLearningRate 0.1
                            |> NeuralNetwork.create
                            |> NeuralNetwork.train (TrainingData [ 1, 0, 1 ] [ 1 ])
                            |> NeuralNetwork.predict { inputs = [ 1, 0, 0 ] }

                    output2 : List Float
                    output2 =
                        neuralNetworkConfig
                            |> NeuralNetwork.withLearningRate 0.100001
                            |> NeuralNetwork.create
                            |> NeuralNetwork.train (TrainingData [ 1, 0, 1 ] [ 1 ])
                            |> NeuralNetwork.predict { inputs = [ 1, 0, 0 ] }
                in
                Expect.notEqual output1 output2
        , test "has correct number of outputs" <|
            \_ ->
                let
                    predictWith : { inputs : Int, outputs : Int } -> Int
                    predictWith { inputs, outputs } =
                        NeuralNetwork.configure
                            { randomSeed = Random.initialSeed 42
                            , inputs = inputs
                            , outputs = outputs
                            }
                            |> NeuralNetwork.create
                            |> NeuralNetwork.predict { inputs = List.repeat inputs 0.5 }
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
                        NeuralNetwork.configure
                            { randomSeed = Random.initialSeed 42
                            , inputs = inputs
                            , outputs = outputs
                            }
                            |> NeuralNetwork.addLayer { neurons = 3 }
                            |> NeuralNetwork.create
                            |> NeuralNetwork.predict { inputs = List.repeat inputs 0.5 }
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
                        NeuralNetwork.configure
                            { randomSeed = Random.initialSeed 42
                            , inputs = 2
                            , outputs = 1
                            }
                            |> NeuralNetwork.create

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
                        NeuralNetwork.configure
                            { randomSeed = Random.initialSeed 42
                            , inputs = List.length trainingData.inputs
                            , outputs = List.length trainingData.expected
                            }
                            |> NeuralNetwork.create
                            |> NeuralNetwork.train trainingData

                    out1 : List Float
                    out1 =
                        neuralNetwork
                            |> NeuralNetwork.predict { inputs = [ 1, 0.1 ] }

                    out2 : List Float
                    out2 =
                        neuralNetwork
                            |> NeuralNetwork.predict { inputs = [ 1, 0.2 ] }
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
                        NeuralNetwork.configure
                            { randomSeed = Random.initialSeed 42
                            , inputs = 2
                            , outputs = 1
                            }
                            |> NeuralNetwork.addLayer { neurons = 6 }
                            |> NeuralNetwork.addLayer { neurons = 4 }
                            --|> NeuralNetwork.withActivationFunction ActivationFunction.Tanh
                            --|> NeuralNetwork.withLearningRate 0.03
                            |> NeuralNetwork.create
                            |> train

                    train : NeuralNetwork -> NeuralNetwork
                    train neuralNetwork_ =
                        randomTrainingDataSet 20000
                            |> List.foldl
                                (\trainingData nn ->
                                    NeuralNetwork.train trainingData nn
                                )
                                neuralNetwork_

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
                            |> Expect.atMost 0.01
                    , \_ ->
                        predict [ 0, 1 ]
                            |> Expect.atLeast 0.99
                    , \_ ->
                        predict [ 1, 0 ]
                            |> Expect.atLeast 0.99
                    , \_ ->
                        predict [ 1, 1 ]
                            |> Expect.atMost 0.01
                    , \_ ->
                        predict [ 0.8, 1 ]
                            |> Expect.atMost 0.01
                    ]
                    neuralNetwork
        , test "can learn 0-3 binary to decimal" <|
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
                                            TrainingData [ 0, 0 ] [ 0, 0, 0 ]

                                        1 ->
                                            TrainingData [ 0, 1 ] [ 0, 0, 1 ]

                                        2 ->
                                            TrainingData [ 1, 0 ] [ 0, 1, 0 ]

                                        _ ->
                                            TrainingData [ 1, 1 ] [ 1, 0, 0 ]
                                )

                    neuralNetwork : NeuralNetwork
                    neuralNetwork =
                        NeuralNetwork.configure
                            { randomSeed = Random.initialSeed 42
                            , inputs = 2
                            , outputs = 3
                            }
                            --|> NeuralNetwork.addLayer { neurons = 10 }
                            --|> NeuralNetwork.withActivationFunction ActivationFunction.Tanh
                            |> NeuralNetwork.withLearningRate 0.3
                            |> NeuralNetwork.create
                            |> train

                    train : NeuralNetwork -> NeuralNetwork
                    train neuralNetwork_ =
                        randomTrainingDataSet 10000
                            |> List.foldl
                                (\trainingData nn ->
                                    NeuralNetwork.train trainingData nn
                                )
                                neuralNetwork_

                    predict : List Float -> List Float
                    predict inputs =
                        neuralNetwork
                            |> NeuralNetwork.predict { inputs = inputs }
                            |> List.map
                                (\x ->
                                    if x < 0.1 then
                                        0

                                    else if x > 0.9 then
                                        1

                                    else
                                        x
                                )
                in
                Expect.all
                    [ \_ ->
                        predict [ 0, 0 ]
                            |> Expect.equalLists [ 0, 0, 0 ]
                    , \_ ->
                        predict [ 0, 1 ]
                            |> Expect.equalLists [ 0, 0, 1 ]
                    , \_ ->
                        predict [ 1, 0 ]
                            |> Expect.equalLists [ 0, 1, 0 ]
                    , \_ ->
                        predict [ 1, 1 ]
                            |> Expect.equalLists [ 1, 0, 0 ]
                    ]
                    neuralNetwork
        , test "can learn 0-7 binary to decimal" <|
            \_ ->
                let
                    randomTrainingDataSet : Int -> List TrainingData
                    randomTrainingDataSet size =
                        Random.step
                            (Random.list size (Random.int 0 9))
                            (Random.initialSeed 666)
                            |> Tuple.first
                            |> List.map
                                (\i ->
                                    case i of
                                        0 ->
                                            TrainingData [ 0, 0, 0 ] [ 0, 0, 0, 0, 0, 0, 0 ]

                                        1 ->
                                            TrainingData [ 0, 0, 1 ] [ 0, 0, 0, 0, 0, 0, 1 ]

                                        2 ->
                                            TrainingData [ 0, 1, 0 ] [ 0, 0, 0, 0, 0, 1, 0 ]

                                        3 ->
                                            TrainingData [ 0, 1, 1 ] [ 0, 0, 0, 0, 1, 0, 0 ]

                                        4 ->
                                            TrainingData [ 1, 0, 0 ] [ 0, 0, 0, 1, 0, 0, 0 ]

                                        5 ->
                                            TrainingData [ 1, 0, 1 ] [ 0, 0, 1, 0, 0, 0, 0 ]

                                        6 ->
                                            TrainingData [ 1, 1, 0 ] [ 0, 1, 0, 0, 0, 0, 0 ]

                                        _ ->
                                            TrainingData [ 1, 1, 1 ] [ 1, 0, 0, 0, 0, 0, 0 ]
                                )

                    neuralNetwork : NeuralNetwork
                    neuralNetwork =
                        NeuralNetwork.configure
                            { randomSeed = Random.initialSeed 42
                            , inputs = 3
                            , outputs = 7
                            }
                            --|> NeuralNetwork.addLayer { neurons = 3 }
                            --|> NeuralNetwork.withActivationFunction ActivationFunction.Tanh
                            |> NeuralNetwork.withLearningRate 0.3
                            |> NeuralNetwork.create
                            |> train

                    train : NeuralNetwork -> NeuralNetwork
                    train neuralNetwork_ =
                        randomTrainingDataSet 30000
                            |> List.foldl
                                (\trainingData nn ->
                                    NeuralNetwork.train trainingData nn
                                )
                                neuralNetwork_

                    predict : List Float -> List Float
                    predict inputs =
                        neuralNetwork
                            |> NeuralNetwork.predict { inputs = inputs }
                            |> List.map
                                (\x ->
                                    if x < 0.1 then
                                        0

                                    else if x > 0.9 then
                                        1

                                    else
                                        x
                                )
                in
                Expect.all
                    [ \_ ->
                        predict [ 0, 0, 0 ]
                            |> Expect.equalLists [ 0, 0, 0, 0, 0, 0, 0 ]
                    , \_ ->
                        predict [ 0, 0, 1 ]
                            |> Expect.equalLists [ 0, 0, 0, 0, 0, 0, 1 ]
                    , \_ ->
                        predict [ 0, 1, 0 ]
                            |> Expect.equalLists [ 0, 0, 0, 0, 0, 1, 0 ]
                    , \_ ->
                        predict [ 0, 1, 1 ]
                            |> Expect.equalLists [ 0, 0, 0, 0, 1, 0, 0 ]
                    ]
                    neuralNetwork
        , test "encodes/decodes" <|
            \_ ->
                let
                    neuralNetwork : NeuralNetwork
                    neuralNetwork =
                        NeuralNetwork.configure
                            { randomSeed = Random.initialSeed 42
                            , inputs = 7
                            , outputs = 4
                            }
                            |> NeuralNetwork.withActivationFunction ActivationFunction.Tanh
                            |> NeuralNetwork.addLayer { neurons = 5 }
                            |> NeuralNetwork.create
                            |> NeuralNetwork.train (TrainingData [ 1, 0, 1, 1, 0, 0, 1 ] [ 0, 0, 1, 1 ])

                    encoded =
                        NeuralNetwork.encode neuralNetwork
                in
                case JD.decodeValue NeuralNetwork.decoder encoded of
                    Ok decoded ->
                        Expect.equal neuralNetwork decoded

                    Err error ->
                        Expect.fail (JD.errorToString error)
        ]
