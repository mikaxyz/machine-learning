module App.NeuralNetworks.Model exposing
    ( Model
    , Msg(..)
    , Page(..)
    )

import Api.Model as Api
import App.NeuralNetworks.Create.Page
import Http
import XYZMika.ML.NeuralNetwork exposing (NeuralNetwork)


type Msg
    = GotModels (Result Http.Error (List Api.Model))
    | CreatePageMsg App.NeuralNetworks.Create.Page.Msg
    | OnCreated String NeuralNetwork


type Page
    = Create App.NeuralNetworks.Create.Page.Model
    | NeuralNetwork Int


type alias Model =
    { models : List Api.Model
    , page : Page
    }
