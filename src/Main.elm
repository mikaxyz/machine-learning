module Main exposing (main)

import App.NeuralNetworks.Page
import Browser
import Html exposing (h1, text)
import Random
import Route exposing (Route)
import Worker.Task
import XYZMika.ML.NeuralNetwork as NeuralNetwork exposing (NeuralNetwork)
import XYZMika.Spa as Spa exposing (Spa)


type Msg
    = SpaMsg Spa.Msg
    | OnRouteChange (Maybe Route)
      --
    | NeuralNetworksPageMsg App.NeuralNetworks.Page.Msg
    | WorkerTaskCompleted Worker.Task.Result


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
                    , subscriptions = \model -> Worker.Task.onCompleted WorkerTaskCompleted
                    , view = \model -> view model
                    , onUrlRequest = \request -> SpaMsg (spa.onUrlRequest request)
                    , onUrlChange = \url -> SpaMsg (spa.onUrlChange url)
                    }
           )


init : ( Spa Route, Cmd Msg ) -> ( Model, Cmd Msg )
init ( spa, spaCmd ) =
    let
        neuralNetwork : NeuralNetwork
        neuralNetwork =
            NeuralNetwork.configure
                { randomSeed = Random.initialSeed 42
                , inputs = 2
                , outputs = 1
                }
                |> NeuralNetwork.addLayer { neurons = 2 }
                |> NeuralNetwork.create
    in
    ( { spa = spa
      , page = NotFound
      }
    , Cmd.batch
        [ spaCmd
        , Worker.Task.TrainNeuralNetwork 200000 neuralNetwork
            |> Worker.Task.start
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WorkerTaskCompleted message ->
            let
                _ =
                    Debug.log "WorkerOnMessage" message
            in
            ( model, Cmd.none )

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
