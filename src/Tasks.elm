module Tasks exposing
    ( TaskError
    , TaskSuccess(..)
    , neuralNetworkToFilename
    , readMnistCsv
    , saveModel
    , saveReport
    )

import ConcurrentTask exposing (ConcurrentTask)
import Json.Decode as JD
import Json.Encode as JE
import XYZMika.ML.ActivationFunction as ActivationFunction
import XYZMika.ML.NeuralNetwork as NeuralNetwork exposing (NeuralNetwork)


type TaskError
    = DecodeError JD.Error
    | IOError


type TaskSuccess
    = TrainingData (List ImageData)
    | ModelSaved String


type alias ImageData =
    { label : Int
    , values : List Int
    }


type alias Report =
    { neuralNetwork : NeuralNetwork
    , content : String
    }


readMnistCsv : String -> ConcurrentTask TaskError (List ImageData)
readMnistCsv fileName =
    ConcurrentTask.define
        { function = "cli:readMnistCsv"
        , expect = ConcurrentTask.expectString
        , errors = ConcurrentTask.expectErrors decodeErrors
        , args = JE.object [ ( "fileName", JE.string fileName ) ]
        }
        |> ConcurrentTask.map (JD.decodeString fileDecoder >> Result.mapError DecodeError)
        |> ConcurrentTask.andThen ConcurrentTask.fromResult


saveModel : NeuralNetwork -> ConcurrentTask TaskError String
saveModel neuralNetwork =
    ConcurrentTask.define
        { function = "cli:saveModel"
        , expect = ConcurrentTask.expectString
        , errors = ConcurrentTask.expectErrors decodeErrors
        , args =
            JE.object
                [ ( "neuralNetwork", NeuralNetwork.encode neuralNetwork )
                , ( "fileName", JE.string (neuralNetworkToFilename neuralNetwork) )
                ]
        }


saveReport : Report -> ConcurrentTask TaskError String
saveReport report =
    ConcurrentTask.define
        { function = "cli:saveReport"
        , expect = ConcurrentTask.expectString
        , errors = ConcurrentTask.expectErrors decodeErrors
        , args =
            JE.object
                [ ( "neuralNetwork", NeuralNetwork.encode report.neuralNetwork )
                , ( "fileName", JE.string (neuralNetworkToFilename report.neuralNetwork) )
                , ( "content", JE.string report.content )
                ]
        }


neuralNetworkToFilename : NeuralNetwork -> String
neuralNetworkToFilename neuralNetwork =
    let
        meta =
            neuralNetwork
                |> NeuralNetwork.meta

        activationFunction =
            case meta.activationFunction of
                ActivationFunction.Tanh ->
                    "tanh"

                ActivationFunction.Sigmoid ->
                    "sigmoid"

        layers =
            meta.layers
                |> List.map String.fromInt
                |> String.join ","
    in
    [ ( "lr", String.fromFloat meta.learningRate )
    , ( "af", activationFunction )
    , ( "in", String.fromInt meta.inputs )
    , ( "out", String.fromInt meta.outputs )
    , ( "l", "[" ++ layers ++ "]" )
    , ( "i", String.fromInt meta.iterations )
    ]
        |> List.map (\( key, value ) -> key ++ "=" ++ value)
        |> String.join ","


imageDataFromString : String -> Result Int ImageData
imageDataFromString x =
    case String.split "," x |> List.map String.toInt of
        (Just label) :: data ->
            { label = label
            , values = data |> List.filterMap identity
            }
                |> Ok

        _ ->
            Err 0


fileDecoder : JD.Decoder (List ImageData)
fileDecoder =
    JD.string
        |> JD.andThen
            (\fileContent ->
                let
                    results : { errors : List Int, images : List ImageData }
                    results =
                        String.lines fileContent
                            |> List.map imageDataFromString
                            |> List.foldl
                                (\x a ->
                                    case x of
                                        Err err ->
                                            { a | errors = err :: a.errors }

                                        Ok image ->
                                            { a | images = image :: a.images }
                                )
                                { errors = [], images = [] }
                in
                case ( results.errors, results.images ) of
                    ( [], [] ) ->
                        JD.fail "No data"

                    ( errors, [] ) ->
                        JD.fail ("Failed to decode " ++ (List.length errors |> String.fromInt) ++ " rows")

                    ( _, images ) ->
                        JD.succeed images
            )


decodeErrors : JD.Decoder TaskError
decodeErrors =
    JD.string
        |> JD.andThen
            (\reason ->
                case reason of
                    "IOError" ->
                        JD.succeed IOError

                    _ ->
                        JD.fail ("Unrecognized ReadError " ++ reason)
            )
