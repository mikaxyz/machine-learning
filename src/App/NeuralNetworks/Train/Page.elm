module App.NeuralNetworks.Train.Page exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Api.Model as Api
import Api.Models
import Html exposing (Html, button, div, fieldset, form, h1, li, progress, section, text, ul)
import Html.Attributes as HA exposing (disabled, type_)
import Html.Events exposing (onSubmit)
import Http
import RemoteData exposing (RemoteData)
import Ui.Input
import Worker.Task
import XYZMika.ML.NeuralNetwork exposing (NeuralNetwork, TrainingData)


type Msg
    = OnChange Form
    | OnNeuralNetwork (Result Http.Error Api.Model)
    | WorkerTaskCompleted Worker.Task.Result
    | WorkerTaskProgress (Result String Worker.Task.Progress)
    | OnSubmit
    | OnSaved
        { title : String
        , neuralNetwork : NeuralNetwork
        }
        (Result Http.Error ())


type Form
    = Inputs String


type alias Model =
    { neuralNetwork : RemoteData String NeuralNetwork
    , trainingData : List TrainingData
    , inputs : String
    }


init : Int -> ( Model, Cmd Msg )
init id =
    ( { neuralNetwork = RemoteData.Loading
      , trainingData = []
      , inputs = ""
      }
    , Api.Models.getById id OnNeuralNetwork
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Worker.Task.onCompleted WorkerTaskCompleted
        , Worker.Task.onProgress WorkerTaskProgress
        ]


update : { tagger : Msg -> msg } -> Msg -> Model -> ( Model, Cmd msg )
update { tagger } msg model =
    case msg of
        OnChange form ->
            ( case form of
                Inputs x ->
                    { model | inputs = x }
            , Cmd.none
            )

        OnSubmit ->
            case model.neuralNetwork of
                RemoteData.Loaded neuralNetwork ->
                    ( { model | neuralNetwork = RemoteData.Progress 0 }
                    , Worker.Task.TrainNeuralNetwork 100 neuralNetwork
                        |> Worker.Task.start
                    )

                _ ->
                    ( model, Cmd.none )

        OnSaved { title, neuralNetwork } result ->
            ( model
            , Cmd.none
            )

        OnNeuralNetwork result ->
            ( case result of
                Ok model_ ->
                    { model | neuralNetwork = RemoteData.Loaded model_.data }

                Err _ ->
                    { model | neuralNetwork = RemoteData.Error "Could not load model" }
            , Cmd.none
            )

        WorkerTaskCompleted result ->
            case result of
                Worker.Task.TrainedNeuralNetwork neuralNetwork ->
                    ( { model | neuralNetwork = RemoteData.Loaded neuralNetwork }
                    , Cmd.none
                    )

                Worker.Task.UnknownResult ->
                    ( model, Cmd.none )

        WorkerTaskProgress (Ok progress) ->
            let
                _ =
                    Debug.log "WorkerTaskProgress" progress
            in
            ( { model | neuralNetwork = RemoteData.Progress progress.complete }
            , Cmd.none
            )

        WorkerTaskProgress (Err _) ->
            ( model
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    section []
        [ h1 [] [ text "Train" ]
        , case model.neuralNetwork of
            RemoteData.Progress x ->
                div []
                    [ progress [ HA.max "1.0", HA.value (String.fromFloat x) ] []

                    --, text (String.fromFloat (x * 100))
                    ]

            _ ->
                text ""
        , form [ onSubmit OnSubmit ]
            [ fieldset [ disabled <| not (RemoteData.isLoaded model.neuralNetwork) ]
                [ model.trainingData
                    |> List.map
                        (\x ->
                            li []
                                [ x.inputs
                                    |> List.map String.fromFloat
                                    |> String.join ","
                                    |> text
                                ]
                        )
                    |> ul []
                , section []
                    [ row { inputs = model.inputs }
                    , button
                        [ type_ "submit" ]
                        [ text "Train" ]
                    ]
                ]
            ]
        ]


row : { inputs : String } -> Html Msg
row { inputs } =
    div []
        [ div []
            [ Ui.Input.input
                { label = "Inputs"
                , value = inputs
                , onChange = Inputs >> OnChange
                }
                |> Ui.Input.toHtml
            ]
        ]
