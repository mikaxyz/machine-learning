module App.NeuralNetworks.Page exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , updateRoute
    , view
    )

import Api.Models
import App.NeuralNetworks.Create.Page
import App.NeuralNetworks.Model as Model exposing (Model, Msg(..), Page)
import App.NeuralNetworks.Route as Route exposing (Route)
import App.NeuralNetworks.Train.Page
import App.NeuralNetworks.View
import Html exposing (Html)
import XYZMika.Spa as Spa exposing (Spa)


init : Route -> ( Model, Cmd Msg )
init route =
    routeToPage route
        |> Tuple.mapBoth
            (\page -> { models = [], page = page })
            (\cmd ->
                Cmd.batch
                    [ Api.Models.get GotModels
                    , cmd
                    ]
            )


updateRoute : Route -> Model -> ( Model, Cmd Msg )
updateRoute route model =
    routeToPage route
        |> Tuple.mapFirst (\page -> { model | page = page })


routeToPage : Route -> ( Page, Cmd Msg )
routeToPage route =
    case route of
        Route.Create ->
            App.NeuralNetworks.Create.Page.init
                |> Tuple.mapFirst Model.Create

        Route.Train id ->
            App.NeuralNetworks.Train.Page.init id
                |> Tuple.mapBoth
                    Model.Train
                    (Cmd.map TrainPageMsg)


type alias Msg =
    Model.Msg


type alias Model =
    Model.Model


view : Model -> Html Msg
view =
    App.NeuralNetworks.View.view


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        Model.Create _ ->
            Sub.none

        Model.Train page ->
            App.NeuralNetworks.Train.Page.subscriptions page
                |> Sub.map TrainPageMsg


update : Spa route -> Msg -> Model -> ( Model, Cmd Msg )
update spa msg model =
    case msg of
        GotModels result ->
            ( case result of
                Ok models ->
                    { model | models = models }

                Err _ ->
                    model
            , Cmd.none
            )

        OnCreated apiModel ->
            ( { model
                | models =
                    { apiModel | title = "NEW: " ++ apiModel.title }
                        :: List.reverse model.models
                        |> List.reverse
              }
            , Spa.pushUrl spa (Route.Train apiModel.id |> Route.toPath)
            )

        CreatePageMsg msg_ ->
            case model.page of
                Model.Create page_ ->
                    App.NeuralNetworks.Create.Page.update
                        { tagger = CreatePageMsg
                        , onCreated = OnCreated
                        }
                        msg_
                        page_
                        |> Tuple.mapFirst
                            (\page -> { model | page = Model.Create page })

                _ ->
                    ( model, Cmd.none )

        TrainPageMsg msg_ ->
            case model.page of
                Model.Train page_ ->
                    App.NeuralNetworks.Train.Page.update
                        { tagger = TrainPageMsg
                        }
                        msg_
                        page_
                        |> Tuple.mapFirst
                            (\page -> { model | page = Model.Train page })

                _ ->
                    ( model, Cmd.none )
