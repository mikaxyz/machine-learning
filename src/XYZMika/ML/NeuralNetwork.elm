module XYZMika.ML.NeuralNetwork exposing
    ( Configuration, configure, addLayer, withActivationFunction, withLearningRate
    , NeuralNetwork, create
    , train, TrainingData
    , predict
    )

{-| Neural Network


# Configuration

@docs Configuration, configure, addLayer, withActivationFunction, withLearningRate


# Create

@docs NeuralNetwork, create


# Train

@docs train, TrainingData


# Use

@docs predict

-}

import Array
import Random
import XYZMika.ML.ActivationFunction as ActivationFunction exposing (ActivationFunction(..))
import XYZMika.ML.Internal.Matrix as Matrix exposing (Matrix)


type alias TrainingData =
    { inputs : List Float
    , expected : List Float
    }


{-| Configuration
Configuration for a NeuralNetwork. Pass one of these to NeuralNetwork.create...
-}
type Configuration
    = Configuration
        { randomSeed : Random.Seed
        , inputs : Int
        , layers : List Int
        , learningRate : Float
        , activationFunction : ActivationFunction
        }


configure :
    { inputs : Int
    , outputs : Int
    , randomSeed : Random.Seed
    }
    -> Configuration
configure config =
    Configuration
        { randomSeed = config.randomSeed
        , inputs = config.inputs
        , layers = [ config.outputs ]
        , learningRate = 0.25
        , activationFunction = Sigmoid
        }


addLayer : { neurons : Int } -> Configuration -> Configuration
addLayer { neurons } (Configuration config) =
    Configuration { config | layers = neurons :: config.layers }


withLearningRate : Float -> Configuration -> Configuration
withLearningRate learningRate (Configuration config) =
    Configuration { config | learningRate = learningRate }


withActivationFunction : ActivationFunction -> Configuration -> Configuration
withActivationFunction activationFunction (Configuration config) =
    Configuration { config | activationFunction = activationFunction }


{-| NeuralNetwork
A NeuralNetwork which can turn inputs onto outputs using using
NeuralNetwork.predict or be trained using NeuralNetwork.train...
-}
type NeuralNetwork
    = NeuralNetwork
        { layers : List Layer
        , learningRate : Float
        , activationFunction : ActivationFunction
        }


type Layer
    = Layer
        { inputCount : Int
        , outputCount : Int
        , weights : Matrix
        , biases : Matrix
        , inputs : Matrix
        , outputs : Matrix
        }


{-| NeuralNetwork
A NeuralNetwork which can turn inputs onto outputs using using
NeuralNetwork.predict or be trained using NeuralNetwork.train...
-}
create : Configuration -> NeuralNetwork
create (Configuration config) =
    let
        ( layers, _, _ ) =
            config.layers
                |> List.foldl
                    (\outputs ( layers_, inputs, seed ) ->
                        let
                            ( Layer layer, seed_ ) =
                                createLayer seed
                                    inputs
                                    outputs
                        in
                        ( Layer layer :: layers_, outputs, seed_ )
                    )
                    ( [], config.inputs, config.randomSeed )
    in
    NeuralNetwork
        { layers = layers |> List.reverse
        , activationFunction = config.activationFunction
        , learningRate = config.learningRate
        }


createLayer :
    Random.Seed
    -> Int
    -> Int
    -> ( Layer, Random.Seed )
createLayer randomSeed inputCount outputCount =
    let
        ( randomWeights, seed1 ) =
            Random.step
                (Random.list (inputCount * outputCount) (Random.float -1 1))
                randomSeed
                |> Tuple.mapFirst Array.fromList

        ( randomBiases, seedOut ) =
            Random.step
                (Random.list (inputCount * outputCount) (Random.float -1 1))
                seed1
                |> Tuple.mapFirst Array.fromList
    in
    ( Layer
        { inputCount = inputCount
        , outputCount = outputCount
        , inputs = Matrix.create ( inputCount, 1 )
        , outputs = Matrix.create ( outputCount, 1 )
        , weights =
            Matrix.create ( outputCount, inputCount )
                |> Matrix.indexedMap
                    (\r c _ ->
                        case Array.get (r * inputCount + c) randomWeights of
                            Just rnd ->
                                rnd

                            Nothing ->
                                -- TODO: Handle this "error"?
                                0
                    )
        , biases =
            Matrix.create ( outputCount, 1 )
                |> Matrix.indexedMap
                    (\r c _ ->
                        case Array.get (r * inputCount + c) randomBiases of
                            Just rnd ->
                                rnd

                            Nothing ->
                                -- TODO: Handle this "error"?
                                0
                    )
        }
    , seedOut
    )



---- TRAINING


{-| Training
Train the NeuralNetwork with some TrainingData
-}
train : TrainingData -> NeuralNetwork -> NeuralNetwork
train trainingData neuralNetwork =
    calculateValues { inputs = trainingData.inputs } neuralNetwork
        |> trainWithValues trainingData


calculateValues : { inputs : List Float } -> NeuralNetwork -> NeuralNetwork
calculateValues config (NeuralNetwork neuralNetwork) =
    NeuralNetwork
        { neuralNetwork
            | layers =
                neuralNetwork.layers
                    |> List.foldl
                        (\(Layer layer) ( layers, inputs_ ) ->
                            let
                                outputs : Matrix
                                outputs =
                                    Matrix.mul layer.weights inputs_
                                        |> Matrix.add layer.biases
                                        |> Matrix.map
                                            (ActivationFunction.func
                                                neuralNetwork.activationFunction
                                            )
                            in
                            ( Layer { layer | inputs = inputs_, outputs = outputs } :: layers, outputs )
                        )
                        ( [], Matrix.fromList (config.inputs |> List.map List.singleton) )
                    |> Tuple.first
                    |> List.reverse
        }


trainWithValues : TrainingData -> NeuralNetwork -> NeuralNetwork
trainWithValues trainingData (NeuralNetwork neuralNetwork) =
    let
        outputs : Matrix
        outputs =
            predictInternal { inputs = trainingData.inputs } (NeuralNetwork neuralNetwork)

        expected : Matrix
        expected =
            Matrix.fromList (trainingData.expected |> List.map List.singleton)

        errors : Matrix
        errors =
            expected |> Matrix.sub outputs
    in
    NeuralNetwork
        { neuralNetwork
            | layers =
                neuralNetwork.layers
                    |> List.reverse
                    |> List.foldl
                        (\layer ( layers, errors_ ) ->
                            let
                                ( trainedLayer, layerErrors ) =
                                    trainLayer
                                        neuralNetwork
                                        errors_
                                        layer
                            in
                            ( trainedLayer :: layers
                            , layerErrors
                            )
                        )
                        ( [], errors )
                    |> Tuple.first
        }


trainLayer :
    { a | learningRate : Float, activationFunction : ActivationFunction }
    -> Matrix
    -> Layer
    -> ( Layer, Matrix )
trainLayer { learningRate, activationFunction } errors (Layer layer) =
    let
        gradients : Matrix
        gradients =
            layer.outputs
                |> Matrix.map (ActivationFunction.dFunc activationFunction)
                |> Matrix.hadamard errors
                |> Matrix.scale learningRate

        deltas : Matrix
        deltas =
            Matrix.mul gradients
                (Matrix.transpose layer.inputs)

        weights : Matrix
        weights =
            layer.weights
                |> Matrix.add deltas

        biases : Matrix
        biases =
            layer.biases |> Matrix.add gradients

        layerErrors =
            Matrix.mul
                (Matrix.transpose layer.weights)
                errors
    in
    ( Layer { layer | weights = weights, biases = biases }
    , layerErrors
    )


{-| Use
Get the outputs from a NeuralNetwork.
-}
predict : { inputs : List Float } -> NeuralNetwork -> List Float
predict config neuralNetwork =
    predictInternal config neuralNetwork
        |> Matrix.toList
        |> List.concat


predictInternal : { inputs : List Float } -> NeuralNetwork -> Matrix
predictInternal config (NeuralNetwork neuralNetwork) =
    let
        inputs =
            Matrix.fromList (config.inputs |> List.map List.singleton)
    in
    List.foldl
        (\(Layer layer) inputs_ ->
            let
                output : Matrix
                output =
                    Matrix.mul layer.weights inputs_
                        |> Matrix.add layer.biases
                        |> Matrix.map
                            (ActivationFunction.func
                                neuralNetwork.activationFunction
                            )
            in
            output
        )
        inputs
        neuralNetwork.layers
