module Api.Models exposing (get, post)

import Api.Model exposing (Model)
import Http
import Json.Decode as JD
import Json.Encode as JE
import XYZMika.ML.NeuralNetwork as NeuralNetwork exposing (NeuralNetwork)


url : String
url =
    "http://localhost:3000/models"


get : (Result Http.Error (List Model) -> msg) -> Cmd msg
get msg =
    Http.get
        { url = url
        , expect = Http.expectJson msg (JD.list Api.Model.decoder)
        }


post : { title : String, neuralNetwork : NeuralNetwork } -> (Result Http.Error () -> msg) -> Cmd msg
post { title, neuralNetwork } msg =
    let
        body =
            JE.object
                [ ( "title", JE.string title )
                , ( "data", NeuralNetwork.encode neuralNetwork )
                ]
    in
    Http.post
        { url = url
        , body = Http.jsonBody body
        , expect = Http.expectWhatever msg
        }
