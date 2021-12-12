module Main exposing (main)

import App.NeuralNetworks.Page
import Browser
import Html exposing (h1, text)
import Route exposing (Route)
import XYZMika.Spa as Spa exposing (Spa)


type Msg
    = SpaMsg Spa.Msg
    | OnRouteChange (Maybe Route)
      --
    | NeuralNetworksPageMsg App.NeuralNetworks.Page.Msg


type Page
    = NotFound
    | NeuralNetworks App.NeuralNetworks.Page.Model


type alias Model =
    { spa : Spa Route
    , page : Page
    }


spaConfig =
    { routeFromUrl = Route.fromUrl
    , routeToPath = Route.toPath
    , onRouteChange = OnRouteChange
    }


main : Program () Model Msg
main =
    Spa.application spaConfig
        |> (\spa ->
                Browser.application
                    { init = \_ url key -> spa.init url key |> init
                    , update = update
                    , subscriptions = \model -> Sub.none
                    , view = \model -> view model
                    , onUrlRequest = \request -> SpaMsg (spa.onUrlRequest request)
                    , onUrlChange = \url -> SpaMsg (spa.onUrlChange url)
                    }
           )


init : ( Spa Route, Cmd Msg ) -> ( Model, Cmd Msg )
init ( spa, spaCmd ) =
    ( { spa = spa
      , page = NotFound
      }
    , spaCmd
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SpaMsg msg_ ->
            Spa.update spaConfig msg_ model.spa
                |> Tuple.mapFirst (\spa -> { model | spa = spa })

        OnRouteChange route ->
            case route of
                Just (Route.NeuralNetworks route_) ->
                    case model.page of
                        NeuralNetworks page_ ->
                            App.NeuralNetworks.Page.updateRoute route_ page_
                                |> Tuple.mapBoth
                                    (\page -> { model | page = NeuralNetworks page })
                                    (Cmd.map NeuralNetworksPageMsg)

                        _ ->
                            App.NeuralNetworks.Page.init route_
                                |> Tuple.mapBoth
                                    (\page -> { model | page = NeuralNetworks page })
                                    (Cmd.map NeuralNetworksPageMsg)

                Nothing ->
                    ( model, Cmd.none )

        NeuralNetworksPageMsg msg_ ->
            case model.page of
                NeuralNetworks page_ ->
                    App.NeuralNetworks.Page.update msg_ page_
                        |> Tuple.mapBoth
                            (\page -> { model | page = NeuralNetworks page })
                            (Cmd.map NeuralNetworksPageMsg)

                _ ->
                    ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Machine Learning"
    , body =
        [ case model.page of
            NeuralNetworks page ->
                App.NeuralNetworks.Page.view page
                    |> Html.map NeuralNetworksPageMsg

            NotFound ->
                h1 [] [ text "404 - Not there" ]
        ]
    }
