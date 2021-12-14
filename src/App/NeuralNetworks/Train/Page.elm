module App.NeuralNetworks.Train.Page exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import Html exposing (Html, button, div, form, h1, li, section, text, ul)
import Html.Attributes exposing (type_)
import Html.Events exposing (onSubmit)
import Http
import Ui.Input
import XYZMika.ML.NeuralNetwork exposing (NeuralNetwork, TrainingData)


type Msg
    = OnChange Form
    | OnSubmit
    | OnSaved
        { title : String
        , neuralNetwork : NeuralNetwork
        }
        (Result Http.Error ())


type Form
    = Inputs String


type alias Model =
    { trainingData : List TrainingData
    , inputs : String
    }


init : Int -> ( Model, Cmd msg )
init id =
    ( { trainingData = []
      , inputs = ""
      }
    , Cmd.none
    )


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
            ( model
            , Cmd.none
            )

        OnSaved { title, neuralNetwork } result ->
            ( model
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    section []
        [ h1 [] [ text "Train" ]
        , form [ onSubmit OnSubmit ]
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
                , button [ type_ "submit" ] [ text "Train" ]
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
