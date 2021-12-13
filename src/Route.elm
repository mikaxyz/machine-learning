module Route exposing (Route(..), fromUrl, toPath)

import App.NeuralNetworks.Route as NeuralNetworks
import Url exposing (Url)
import Url.Parser exposing ((</>), Parser, s)


type Route
    = NeuralNetworks NeuralNetworks.Route


parser : Parser (Route -> a) a
parser =
    Url.Parser.oneOf
        [ Url.Parser.map NeuralNetworks NeuralNetworks.parser
        ]


toPath : Route -> String
toPath route =
    case route of
        NeuralNetworks route_ ->
            "/neural-networks" ++ NeuralNetworks.toPath route_


fromUrl : Url -> Maybe Route
fromUrl url =
    Url.Parser.parse parser url
