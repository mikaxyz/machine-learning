module Commands.Test exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    )

import ConcurrentTask exposing (ConcurrentTask)
import Dict exposing (Dict)
import Json.Decode as JD
import Port
import Task
import Tasks exposing (TaskError, TaskSuccess(..))
import XYZMika.Debug
import XYZMika.ML.NeuralNetwork as NeuralNetwork exposing (NeuralNetwork)


type alias Flags =
    { neuralNetwork : NeuralNetwork
    , testDataPath : String
    }


flagsDecoder : JD.Decoder Flags
flagsDecoder =
    JD.map2 Flags
        (JD.field "model" NeuralNetwork.decoder)
        (JD.field "testDataPath" JD.string)


type Msg
    = OnProgress ( ConcurrentTask.Pool Msg TaskError TaskSuccess, Cmd Msg )
    | OnComplete (ConcurrentTask.Response TaskError TaskSuccess)
    | Test ImageData
    | TestWithData


type alias Model =
    { tasks : ConcurrentTask.Pool Msg TaskError TaskSuccess
    , neuralNetwork : NeuralNetwork
    , testData : List ImageData
    , results : Dict Int (List Bool)
    }


type alias ImageData =
    { label : Int
    , values : List Int
    }


init : JD.Value -> ( Result JD.Error Model, Cmd Msg )
init flagsValue =
    case JD.decodeValue flagsDecoder flagsValue of
        Ok flags ->
            initWithFlags flags |> Tuple.mapFirst Ok

        Err error ->
            let
                _ =
                    XYZMika.Debug.log "Failed to decode flags for train command" (JD.errorToString error)
            in
            ( Err error, Cmd.none )


initWithFlags : Flags -> ( Model, Cmd Msg )
initWithFlags flags =
    let
        ( tasks, cmd ) =
            ConcurrentTask.attempt
                { send = Port.send
                , pool = ConcurrentTask.pool
                , onComplete = OnComplete
                }
                (Tasks.readMnistCsv flags.testDataPath |> ConcurrentTask.map TrainingData)
    in
    ( { tasks = tasks
      , neuralNetwork = flags.neuralNetwork
      , testData = []
      , results = Dict.empty
      }
    , cmd
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    ConcurrentTask.onProgress
        { send = Port.send
        , receive = Port.receive
        , onProgress = OnProgress
        }
        model.tasks


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnComplete (ConcurrentTask.Success (TrainingData data)) ->
            let
                _ =
                    XYZMika.Debug.log "TestWithData" (List.length data)
            in
            ( { model | testData = data }
            , Task.succeed () |> Task.perform (\_ -> TestWithData)
            )

        OnComplete (ConcurrentTask.Success (ModelSaved path)) ->
            let
                _ =
                    XYZMika.Debug.log "Saved report: " path
            in
            ( model
            , Cmd.none
            )

        OnComplete error ->
            let
                _ =
                    XYZMika.Debug.log "ERROR" error
            in
            ( model, Cmd.none )

        OnProgress ( tasks, cmd ) ->
            ( { model | tasks = tasks }, cmd )

        TestWithData ->
            case model.testData of
                current :: rest ->
                    ( { model | testData = rest }
                    , Task.succeed current |> Task.perform Test
                    )

                [] ->
                    let
                        percentagesByLabel : Dict Int Float
                        percentagesByLabel =
                            model.results
                                |> Dict.map
                                    (\_ values ->
                                        let
                                            total =
                                                values
                                                    |> List.length
                                                    |> toFloat

                                            correct =
                                                values
                                                    |> List.filter identity
                                                    |> List.length
                                                    |> toFloat
                                        in
                                        correct / total
                                    )

                        decimals =
                            2

                        labels : List { label : Int, total : Int, correct : Int }
                        labels =
                            model.results
                                |> Dict.map
                                    (\label values ->
                                        let
                                            correct : Int
                                            correct =
                                                values
                                                    |> List.filter identity
                                                    |> List.length
                                        in
                                        { label = label, total = List.length values, correct = correct }
                                    )
                                |> Dict.toList
                                |> List.map Tuple.second

                        totalNumberOfDocuments : Int
                        totalNumberOfDocuments =
                            labels
                                |> List.map .total
                                |> List.sum

                        byLabel : { label : Int, total : Int, correct : Int } -> String
                        byLabel { label, total, correct } =
                            let
                                ratio =
                                    toFloat correct / toFloat total
                            in
                            (String.fromInt label ++ alignRightBy 15 (floatToPercentageDisplay decimals ratio)) ++ alignRight (String.fromInt total)

                        alignRightBy : Int -> String -> String
                        alignRightBy n x =
                            String.padLeft n ' ' x

                        alignRight : String -> String
                        alignRight x =
                            String.padLeft 10 ' ' x

                        totals : String
                        totals =
                            (Dict.values percentagesByLabel |> List.sum)
                                / (Dict.size percentagesByLabel |> toFloat)
                                |> floatToPercentageDisplay decimals
                                |> (\a -> "TOTAL:" ++ alignRight a ++ alignRight (String.fromInt totalNumberOfDocuments))

                        labelsReport =
                            labels
                                |> List.map byLabel
                                |> String.join "\n"

                        title =
                            Tasks.neuralNetworkToFilename model.neuralNetwork

                        report =
                            [ title
                            , "--------------------------"
                            , "LABEL   ACCURACY     COUNT"
                            , labelsReport
                            , "--------------------------"
                            , totals
                            ]
                                |> String.join "\n"

                        ( tasks, cmd ) =
                            ConcurrentTask.attempt
                                { send = Port.send
                                , pool = ConcurrentTask.pool
                                , onComplete = OnComplete
                                }
                                (Tasks.saveReport { neuralNetwork = model.neuralNetwork, content = report } |> ConcurrentTask.map ModelSaved)
                    in
                    ( { model | tasks = tasks }
                    , cmd
                    )

        Test imageData ->
            let
                prediction : List Float
                prediction =
                    NeuralNetwork.predict
                        { inputs = imageData.values |> List.map (\x -> toFloat x / 255)
                        }
                        model.neuralNetwork

                cleanPrediction : List Int
                cleanPrediction =
                    prediction
                        --|> predictionWithTolerance 0.7
                        |> predictionWithBestGuess

                optimal : List Int
                optimal =
                    labelToOutput10 imageData.label

                result =
                    cleanPrediction == optimal
            in
            ( { model
                | results =
                    model.results
                        |> Dict.update imageData.label
                            (\maybeValues ->
                                case maybeValues of
                                    Just values ->
                                        result :: values |> Just

                                    Nothing ->
                                        Just [ result ]
                            )
              }
            , Task.succeed () |> Task.perform (\_ -> TestWithData)
            )


floatToPercentageDisplay : Int -> Float -> String
floatToPercentageDisplay d x =
    let
        decs a =
            String.left d a |> String.padRight d '0'

        ints a =
            String.left 3 a |> String.padLeft 3 ' '
    in
    case String.fromFloat (100 * x) |> String.split "." of
        [ integer, decimals ] ->
            ints integer ++ "." ++ decs decimals ++ "%"

        [ integer ] ->
            ints integer ++ "." ++ decs "000000000000000" ++ "%"

        _ ->
            "INVALID"


floatToDisplay : Int -> Float -> String
floatToDisplay d x =
    let
        decs a =
            String.left d a |> String.padRight d '0'
    in
    case String.fromFloat x |> String.split "." of
        [ integer, decimals ] ->
            integer ++ "." ++ decs decimals

        _ ->
            String.fromFloat x ++ "." ++ decs "000000000000000"


predictionWithTolerance : Float -> List Float -> List Int
predictionWithTolerance tolerance prediction =
    prediction
        |> List.map
            (\x ->
                if x > tolerance then
                    1

                else
                    0
            )


predictionWithBestGuess : List Float -> List Int
predictionWithBestGuess prediction =
    let
        max =
            prediction |> List.maximum
    in
    prediction
        |> List.map
            (\x ->
                if Just x == max then
                    1

                else
                    0
            )


labelToOutput10 : Int -> List number
labelToOutput10 label =
    case label of
        0 ->
            [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 ]

        1 ->
            [ 0, 0, 0, 0, 0, 0, 0, 0, 1, 0 ]

        2 ->
            [ 0, 0, 0, 0, 0, 0, 0, 1, 0, 0 ]

        3 ->
            [ 0, 0, 0, 0, 0, 0, 1, 0, 0, 0 ]

        4 ->
            [ 0, 0, 0, 0, 0, 1, 0, 0, 0, 0 ]

        5 ->
            [ 0, 0, 0, 0, 1, 0, 0, 0, 0, 0 ]

        6 ->
            [ 0, 0, 0, 1, 0, 0, 0, 0, 0, 0 ]

        7 ->
            [ 0, 0, 1, 0, 0, 0, 0, 0, 0, 0 ]

        8 ->
            [ 0, 1, 0, 0, 0, 0, 0, 0, 0, 0 ]

        _ ->
            [ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
