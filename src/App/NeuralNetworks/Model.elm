module App.NeuralNetworks.Model exposing
    ( Model
    , Msg(..)
    , Page(..)
    )

import Api.Model as Api
import App.NeuralNetworks.Create.Page
import App.NeuralNetworks.Train.Page
import Http


type Msg
    = GotModels (Result Http.Error (List Api.Model))
    | CreatePageMsg App.NeuralNetworks.Create.Page.Msg
    | TrainPageMsg App.NeuralNetworks.Train.Page.Msg
    | OnCreated Api.Model


type Page
    = Create App.NeuralNetworks.Create.Page.Model
    | Train App.NeuralNetworks.Train.Page.Model


type alias Model =
    { models : List Api.Model
    , page : Page
    }
