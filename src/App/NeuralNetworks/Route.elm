module App.NeuralNetworks.Route exposing (Route(..), fromUrl, parser, toPath)

import Url exposing (Url)
import Url.Parser exposing ((</>), Parser)


type Route
    = Create
    | NeuralNetwork Int


parser : Parser (Route -> a) a
parser =
    Url.Parser.oneOf
        [ Url.Parser.map Create Url.Parser.top
        , Url.Parser.map NeuralNetwork Url.Parser.int
        ]


toPath : Route -> String
toPath route =
    case route of
        Create ->
            "/"

        NeuralNetwork x ->
            "/" ++ String.fromInt x


fromUrl : Url -> Maybe Route
fromUrl url =
    Url.Parser.parse parser url
