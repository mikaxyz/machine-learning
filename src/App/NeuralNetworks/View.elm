module App.NeuralNetworks.View exposing (view)

import App.NeuralNetworks.Create.Page
import App.NeuralNetworks.Model as Model exposing (Model, Msg(..))
import App.NeuralNetworks.Train.Page
import App.NeuralNetworks.View.Page
import Html exposing (..)
import Html.Attributes exposing (..)


view : Model -> Html Msg
view model =
    main_ [ class "app" ]
        [ div [ class "app__viewport" ]
            [ case model.page of
                Model.Create model_ ->
                    App.NeuralNetworks.Create.Page.view model_
                        |> Html.map CreatePageMsg

                Model.Train model_ ->
                    App.NeuralNetworks.Train.Page.view model_
                        |> Html.map TrainPageMsg

                Model.View model_ ->
                    App.NeuralNetworks.View.Page.view model_
                        |> Html.map ViewPageMsg
            ]
        , aside
            [ class "app__sidebar" ]
            [ nav []
                [ h2 [] []
                , a [ href "/" ] [ text "Create new" ]
                , h2 [] [ text "Models" ]
                , model.models
                    |> List.reverse
                    |> List.map
                        (\x ->
                            li []
                                [ a [ href <| "/view/" ++ String.fromInt x.id ] [ text x.title ]
                                ]
                        )
                    |> ul []
                ]
            ]
        ]
