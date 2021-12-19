port module Worker.Task exposing
    ( Progress
    , Result(..)
    , Task(..)
    , complete
    , onCompleted
    , onProgress
    , onStart
    , progress
    , start
    )

import Json.Decode as JD
import Json.Encode as JE
import XYZMika.ML.NeuralNetwork as NeuralNetwork exposing (NeuralNetwork)


port workerTaskStart : JE.Value -> Cmd msg


port workerTaskOnStart : (JE.Value -> msg) -> Sub msg


port workerTaskComplete : JE.Value -> Cmd msg


port workerOnCompleted : (JE.Value -> msg) -> Sub msg


port workerTaskProgress : JE.Value -> Cmd msg


port workerOnProgress : (JE.Value -> msg) -> Sub msg


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


progress : Progress -> Cmd msg
progress data =
    workerTaskProgress (encodeProgress data)


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


onProgress : (Result.Result String Progress -> msg) -> Sub msg
onProgress msg =
    workerOnProgress
        (\value ->
            value
                |> JD.decodeValue progressDecoder
                |> Result.mapError (always "Progress data was broken somehow...")
                |> msg
        )


type alias Progress =
    { id : Int, complete : Float, neuralNetwork : NeuralNetwork }


type Task
    = UnknownTask
    | TrainNeuralNetwork Int NeuralNetwork


type Result
    = UnknownResult
    | TrainedNeuralNetwork NeuralNetwork


encodeProgress : Progress -> JE.Value
encodeProgress data =
    JE.object
        [ ( "id", JE.int data.id )
        , ( "complete", JE.float data.complete )
        , ( "neuralNetwork", NeuralNetwork.encode data.neuralNetwork )
        ]


progressDecoder : JD.Decoder Progress
progressDecoder =
    JD.map3 Progress
        (JD.field "id" JD.int)
        (JD.field "complete" JD.float)
        (JD.field "neuralNetwork" NeuralNetwork.decoder)


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
