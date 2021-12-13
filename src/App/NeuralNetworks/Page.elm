module App.NeuralNetworks.Page exposing
    ( Model
    , Msg
    , init
    , update
    , updateRoute
    , view
    )

import Api.Models
import App.NeuralNetworks.Create.Page
import App.NeuralNetworks.Model as Model exposing (Model, Msg(..), Page)
import App.NeuralNetworks.Route as Route exposing (Route)
import App.NeuralNetworks.View
import Html exposing (Html)


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

        Route.NeuralNetwork id ->
            ( Model.NeuralNetwork id, Cmd.none )


type alias Msg =
    Model.Msg


type alias Model =
    Model.Model


view : Model -> Html Msg
view =
    App.NeuralNetworks.View.view


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotModels result ->
            ( case result of
                Ok models ->
                    { model | models = models }

                Err _ ->
                    model
            , Cmd.none
            )

        OnCreated title neuralNetwork ->
            ( { model
                | models =
                    { id = -1, title = title, data = neuralNetwork }
                        :: List.reverse model.models
                        |> List.reverse
              }
            , Cmd.none
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
