module App.NeuralNetworks.Create.Page exposing (Model, Msg, init, update, view)

import Api.Model as Api
import Api.Models
import Html exposing (Html, button, form, h1, section, text)
import Html.Attributes exposing (type_)
import Html.Events exposing (onSubmit)
import Http
import Random
import Task
import Ui.Input
import Ui.Select
import XYZMika.ML.ActivationFunction as ActivationFunction exposing (ActivationFunction)
import XYZMika.ML.NeuralNetwork as NeuralNetwork exposing (NeuralNetwork)


type Msg
    = OnChange Form
    | OnSubmit
    | OnSaved (Result Http.Error Api.Model)


type Form
    = Inputs String
    | Outputs String
    | LearningRate String
    | ActivationFunction String


activationFunctionToString : ActivationFunction -> String
activationFunctionToString x =
    case x of
        ActivationFunction.Sigmoid ->
            "Sigmoid"

        ActivationFunction.Tanh ->
            "Tanh"


activationFunctionFromString : String -> Maybe ActivationFunction
activationFunctionFromString x =
    case x of
        "Sigmoid" ->
            Just ActivationFunction.Sigmoid

        "Tanh" ->
            Just ActivationFunction.Tanh

        _ ->
            Nothing


type alias Model =
    { inputs : Int
    , outputs : Int
    , learningRate : Float
    , activationFunction : ActivationFunction
    }


init : ( Model, Cmd msg )
init =
    ( { inputs = 2
      , outputs = 1
      , learningRate = 0.25
      , activationFunction = ActivationFunction.Sigmoid
      }
    , Cmd.none
    )


update : { tagger : Msg -> msg, onCreated : Api.Model -> msg } -> Msg -> Model -> ( Model, Cmd msg )
update { tagger, onCreated } msg model =
    case msg of
        OnChange form ->
            ( case form of
                Inputs x ->
                    { model | inputs = String.toInt x |> Maybe.withDefault model.inputs }

                Outputs x ->
                    { model | outputs = String.toInt x |> Maybe.withDefault model.outputs }

                LearningRate x ->
                    { model | learningRate = String.toFloat x |> Maybe.withDefault model.learningRate }

                ActivationFunction x ->
                    { model
                        | activationFunction =
                            activationFunctionFromString x
                                |> Maybe.withDefault ActivationFunction.Sigmoid
                    }
            , Cmd.none
            )

        OnSubmit ->
            let
                title =
                    "NeuralNetwork with " ++ String.fromInt model.inputs ++ " inputs"

                neuralNetwork : NeuralNetwork
                neuralNetwork =
                    NeuralNetwork.configure
                        { inputs = model.inputs
                        , outputs = model.outputs
                        , randomSeed = Random.initialSeed 42 -- TODO...
                        }
                        |> NeuralNetwork.withLearningRate model.learningRate
                        |> NeuralNetwork.withActivationFunction model.activationFunction
                        |> NeuralNetwork.create
            in
            ( model
            , Api.Models.post
                { title = title
                , neuralNetwork = neuralNetwork
                }
                OnSaved
                |> Cmd.map tagger
            )

        OnSaved result ->
            case result of
                Ok apiModel ->
                    ( model
                    , Task.perform onCreated (Task.succeed apiModel)
                    )

                Err error ->
                    ( model
                    , Cmd.none
                    )


view : Model -> Html Msg
view model =
    section []
        [ h1 [] [ text "Create" ]
        , form [ onSubmit OnSubmit ]
            [ Ui.Input.input
                { label = "Inputs"
                , value = model.inputs |> String.fromInt
                , onChange = Inputs >> OnChange
                }
                |> Ui.Input.withNumberRange 2 42
                |> Ui.Input.toHtml
            , Ui.Input.input
                { label = "Outputs"
                , value = model.outputs |> String.fromInt
                , onChange = Outputs >> OnChange
                }
                |> Ui.Input.withNumberRange 1 12
                |> Ui.Input.toHtml
            , Ui.Input.input
                { label = "Learning Rate"
                , value = model.learningRate |> String.fromFloat
                , onChange = LearningRate >> OnChange
                }
                |> Ui.Input.withNumberRangeAndStep 0 1 0.01
                |> Ui.Input.toHtml
            , Ui.Select.select
                { label = "Activation function"
                , value = model.activationFunction |> activationFunctionToString
                , onChange = ActivationFunction >> OnChange
                , options =
                    [ Ui.Select.option { value = "Sigmoid", label = "Sigmoid" }
                    , Ui.Select.option { value = "Tanh", label = "Tanh" }
                    ]
                }
                |> Ui.Select.toHtml
            , button [ type_ "submit" ] [ text "Create" ]
            ]
        ]
