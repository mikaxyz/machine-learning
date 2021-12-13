module Api.Model exposing (Model, decoder)

import Json.Decode as JD
import XYZMika.ML.NeuralNetwork as NeuralNetwork exposing (NeuralNetwork)


type alias Model =
    { id : Int
    , title : String
    , data : NeuralNetwork
    }


decoder : JD.Decoder Model
decoder =
    JD.map3 Model
        (JD.field "id" JD.int)
        (JD.field "title" JD.string)
        (JD.field "data" NeuralNetwork.decoder)
