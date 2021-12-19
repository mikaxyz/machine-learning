module App.NeuralNetworks.Route exposing (Route(..), fromUrl, parser, toPath)

import Url exposing (Url)
import Url.Parser exposing ((</>), Parser, s)


type Route
    = Create
    | Train Int
    | View Int


parser : Parser (Route -> a) a
parser =
    Url.Parser.oneOf
        [ Url.Parser.map Create Url.Parser.top
        , Url.Parser.map Train (s "train" </> Url.Parser.int)
        , Url.Parser.map View (s "view" </> Url.Parser.int)
        ]


toPath : Route -> String
toPath route =
    case route of
        Create ->
            "/"

        Train x ->
            "/train/" ++ String.fromInt x

        View x ->
            "/view/" ++ String.fromInt x


fromUrl : Url -> Maybe Route
fromUrl url =
    Url.Parser.parse parser url
