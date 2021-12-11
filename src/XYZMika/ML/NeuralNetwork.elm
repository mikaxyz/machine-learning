module XYZMika.ML.NeuralNetwork exposing
    ( configure, addLayer
    , create
    , train, TrainingData
    , predict
    , Configuration, NeuralNetwork
    )

{-| Neural Network


# Configuration

@docs configure, addLayer


# Create

@docs create


# Train

@docs train, TrainingData


# Use

@docs predict

-}

import Array
import Random
import XYZMika.ML.Matrix as Matrix exposing (Matrix)


logger : Bool -> String -> a -> a
logger on msg x =
    -- TODO: Remove...
    if on then
        Debug.log msg x

    else
        x


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


type ActivationFunction
    = Sigmoid


getActivationFunction : ActivationFunction -> (Float -> Float)
getActivationFunction x =
    -- TODO: This no gooood....
    case x of
        Sigmoid ->
            sigmoid


sigmoid : Float -> Float
sigmoid x =
    1 / (1 + Basics.e ^ -x)


deSigmoid : Float -> Float
deSigmoid y =
    y * (1 - y)


{-| NeuralNetwork
A NeuralNetwork which can turn inputs onto outputs using using
NeuralNetwork.predict or be trained using NeuralNetwork.train...
-}
type NeuralNetwork
    = NeuralNetwork
        { randomSeed : Random.Seed
        , layers : List Layer
        , activationFunction : Float -> Float
        , learningRate : Float
        }


type Layer
    = Layer
        { inputCount : Int
        , outputCount : Int
        , weights : Matrix
        , biases : Matrix
        , inputs : Matrix
        , outputs : Matrix
        , activationFunction : Float -> Float
        }


{-| NeuralNetwork
A NeuralNetwork which can turn inputs onto outputs using using
NeuralNetwork.predict or be trained using NeuralNetwork.train...
-}
create : Configuration -> NeuralNetwork
create (Configuration config) =
    let
        -- TODO: Fix this. Should not be stored in "model"
        activationFunction =
            getActivationFunction config.activationFunction

        ( layers, randomSeed ) =
            config.layers
                |> List.reverse
                |> List.foldl
                    (\outputs ( layers_, seed ) ->
                        let
                            ( layer, seed_ ) =
                                createLayer seed
                                    activationFunction
                                    config.inputs
                                    outputs
                        in
                        ( layer :: layers_, seed_ )
                    )
                    ( [], config.randomSeed )
    in
    NeuralNetwork
        { randomSeed = randomSeed
        , layers = layers
        , activationFunction = activationFunction
        , learningRate = config.learningRate
        }


createLayer :
    Random.Seed
    -> (Float -> Float)
    -> Int
    -> Int
    -> ( Layer, Random.Seed )
createLayer randomSeed activationFunction inputCount outputCount =
    let
        log =
            logger True

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
                                log "RANDOM WEIGHT MISSING!" 0
                    )
        , biases =
            Matrix.create ( outputCount, 1 )
                |> Matrix.indexedMap
                    (\r c _ ->
                        case Array.get (r * inputCount + c) randomBiases of
                            Just rnd ->
                                rnd

                            Nothing ->
                                log "RANDOM BIAS MISSING!" 0
                    )
        , activationFunction = activationFunction
        }
    , seedOut
    )



---- TRAINING


{-| Training
Train the NeuralNetwork with some TrainingData
-}
train : TrainingData -> NeuralNetwork -> NeuralNetwork
train trainingData neuralNetwork =
    let
        log =
            logger False

        _ =
            log "------------- TRAIN IT" trainingData
    in
    calculateValues { inputs = trainingData.inputs } neuralNetwork
        |> trainWithValues trainingData


calculateValues : { inputs : List Float } -> NeuralNetwork -> NeuralNetwork
calculateValues config (NeuralNetwork neuralNetwork) =
    let
        log =
            logger False

        _ =
            log "-> calculateValues" config

        inputs =
            Matrix.fromList (config.inputs |> List.map List.singleton)
    in
    NeuralNetwork
        { neuralNetwork
            | layers =
                neuralNetwork.layers
                    |> List.foldl
                        (\(Layer layer) ( layers, inputs_ ) ->
                            let
                                _ =
                                    log "- - - INPUTS" inputs_

                                _ =
                                    log "layer.weights" layer.weights

                                _ =
                                    log "layer.biases" layer.biases

                                _ =
                                    log "- - - OUTPUTS" outputs

                                outputs : Matrix
                                outputs =
                                    Matrix.mul layer.weights inputs_
                                        |> Matrix.add layer.biases
                                        |> Matrix.map neuralNetwork.activationFunction
                            in
                            ( Layer { layer | inputs = inputs_, outputs = outputs } :: layers, outputs )
                        )
                        ( [], inputs )
                    |> Tuple.first
                    |> List.reverse
        }


trainWithValues : TrainingData -> NeuralNetwork -> NeuralNetwork
trainWithValues trainingData (NeuralNetwork neuralNetwork) =
    let
        log =
            logger False

        _ =
            log "_________________________________________" ""

        _ =
            log "-> trainWithValues" trainingData

        outputs : Matrix
        outputs =
            log "OUTPUTS" <|
                predict { inputs = trainingData.inputs } (NeuralNetwork neuralNetwork)

        expected : Matrix
        expected =
            log "EXPECTED" <|
                Matrix.fromList (trainingData.expected |> List.map List.singleton)

        errors : Matrix
        errors =
            expected |> Matrix.sub outputs

        _ =
            neuralNetwork.layers
                |> List.indexedMap
                    (\index (Layer layer) ->
                        ( index, layer.inputs )
                    )
                |> log ">>>>>>>>>>>>>>>>"
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
                                    trainLayer neuralNetwork.learningRate errors_ layer
                            in
                            ( trainedLayer :: layers
                            , layerErrors
                            )
                        )
                        ( [], errors )
                    |> Tuple.first
        }


trainLayer : Float -> Matrix -> Layer -> ( Layer, Matrix )
trainLayer learningRate errors_ (Layer layer) =
    let
        log =
            logger False

        deActivationFunction =
            -- TODO: Add to Config...
            deSigmoid

        _ =
            log "------------------ TRAIN LAYER ---------------------" 0

        _ =
            log "INPUTS" (Matrix.toList layer.inputs)

        _ =
            log "OUTPUTS" (Matrix.toList layer.outputs)

        _ =
            log "errors_" (Matrix.toList errors_)

        _ =
            log "OLD WEIGHTS" (Matrix.toList layer.weights)

        _ =
            log "NEW WEIGHTS" (Matrix.toList weights)

        _ =
            log "GRADIENTS" (Matrix.toList gradients)

        _ =
            log "DELTAS" deltas

        _ =
            if Matrix.dimensions layer.weights /= Matrix.dimensions weights then
                -- TODO: Return Result?
                log "        !!!!!!! WRONG DIMENSIONS !!!!!!!       " 0

            else
                1

        gradients : Matrix
        gradients =
            layer.outputs
                |> Matrix.map deActivationFunction
                |> Matrix.hadamard errors_
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
                errors_
    in
    ( Layer
        { layer
            | weights = weights
            , biases = biases
        }
    , layerErrors
    )


{-| Use
Get the outputs from a NeuralNetwork.
-}
predict : { inputs : List Float } -> NeuralNetwork -> Matrix
predict config (NeuralNetwork neuralNetwork) =
    let
        log =
            logger False

        inputs =
            Matrix.fromList (config.inputs |> List.map List.singleton)
                |> log "------------- PREDICT"
    in
    List.foldl
        (\(Layer layer) ( index, inputs_ ) ->
            let
                _ =
                    log ("INPUTS LAYER" ++ String.fromInt index) inputs_

                _ =
                    log "layer.weights" layer.weights

                output : Matrix
                output =
                    Matrix.mul layer.weights inputs_
                        |> log "INPUTS AFTER WEIGHT"
                        |> Matrix.add (log "layer.biases" layer.biases)
                        |> Matrix.map neuralNetwork.activationFunction
                        |> log ("OUTPUT LAYER" ++ String.fromInt index)
            in
            ( index + 1, output )
        )
        ( 1, inputs )
        neuralNetwork.layers
        |> Tuple.second
