port module Worker.Task exposing
    ( Result(..)
    , Task(..)
    , complete
    , onCompleted
    , onStart
    , start
    )

import Json.Decode as JD
import Json.Encode as JE
import XYZMika.ML.NeuralNetwork as NeuralNetwork exposing (NeuralNetwork)


port workerTaskStart : JE.Value -> Cmd msg


port workerTaskOnStart : (JE.Value -> msg) -> Sub msg


port workerTaskComplete : JE.Value -> Cmd msg


port workerOnCompleted : (JE.Value -> msg) -> Sub msg


start : Task -> Cmd msg
start task =
    workerTaskStart (encodeTask task)


onStart : (Task -> msg) -> Sub msg
onStart msg =
    workerTaskOnStart
        (\value ->
            case JD.decodeValue taskDecoder value of
                Ok task ->
                    msg task

                Err _ ->
                    msg UnknownTask
        )


complete : Result -> Cmd msg
complete result =
    workerTaskComplete (encodeResult result)


onCompleted : (Result -> msg) -> Sub msg
onCompleted msg =
    workerOnCompleted
        (\value ->
            case JD.decodeValue resultDecoder value of
                Ok incoming ->
                    msg incoming

                Err _ ->
                    msg UnknownResult
        )


type Task
    = UnknownTask
    | TrainNeuralNetwork Int NeuralNetwork


type Result
    = UnknownResult
    | TrainedNeuralNetwork NeuralNetwork


encodeTask : Task -> JE.Value
encodeTask task =
    case task of
        UnknownTask ->
            JE.null

        TrainNeuralNetwork steps neuralNetwork ->
            JE.object
                [ ( "steps", JE.int steps )
                , ( "trainNeuralNetwork", NeuralNetwork.encode neuralNetwork )
                ]


taskDecoder : JD.Decoder Task
taskDecoder =
    JD.oneOf
        [ JD.map2 TrainNeuralNetwork
            (JD.field "steps" JD.int)
            (JD.field "trainNeuralNetwork" NeuralNetwork.decoder)
        ]


encodeResult : Result -> JE.Value
encodeResult result =
    case result of
        UnknownResult ->
            JE.null

        TrainedNeuralNetwork neuralNetwork ->
            JE.object
                [ ( "trainedNeuralNetwork", NeuralNetwork.encode neuralNetwork )
                ]


resultDecoder : JD.Decoder Result
resultDecoder =
    JD.oneOf
        [ JD.map TrainedNeuralNetwork
            (JD.field "trainedNeuralNetwork" NeuralNetwork.decoder)
        ]
