module Tasks exposing
    ( TaskError
    , TaskSuccess(..)
    , readMnistCsv
    , saveModel
    )

import ConcurrentTask exposing (ConcurrentTask)
import Json.Decode as JD
import Json.Encode as JE
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
        , args = NeuralNetwork.encode neuralNetwork
        }


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
