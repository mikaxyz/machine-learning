module Api.Models exposing (get, getById, post)

import Api.Model exposing (Model)
import Http
import Json.Decode as JD
import Json.Encode as JE
import XYZMika.ML.NeuralNetwork as NeuralNetwork exposing (NeuralNetwork)


url : String
url =
    "http://localhost:3000/models"


getById : Int -> (Result Http.Error Model -> msg) -> Cmd msg
getById id msg =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Accept" "application/vnd.pgrst.object+json" ]
        , url = url ++ "?id=eq." ++ String.fromInt id
        , body = Http.emptyBody
        , expect = Http.expectJson msg Api.Model.decoder
        , timeout = Nothing
        , tracker = Nothing
        }


get : (Result Http.Error (List Model) -> msg) -> Cmd msg
get msg =
    Http.get
        { url = url
        , expect = Http.expectJson msg (JD.list Api.Model.decoder)
        }


post : { title : String, neuralNetwork : NeuralNetwork } -> (Result Http.Error Model -> msg) -> Cmd msg
post { title, neuralNetwork } msg =
    let
        body =
            JE.object
                [ ( "title", JE.string title )
                , ( "data", NeuralNetwork.encode neuralNetwork )
                ]
    in
    Http.request
        { method = "POST"
        , headers =
            [ Http.header "Accept" "application/vnd.pgrst.object+json"
            , Http.header "Prefer" "return=representation"
            ]
        , url = url
        , body = Http.jsonBody body
        , expect = Http.expectJson msg Api.Model.decoder
        , timeout = Nothing
        , tracker = Nothing
        }
