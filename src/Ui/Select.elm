module Ui.Select exposing
    ( option
    , select
    , toHtml
    )

import Html exposing (..)
import Html.Attributes as HA exposing (..)
import Html.Events exposing (onInput)


type Select msg
    = Select (Config msg)


type Option
    = Option { value : String, label : String }


type alias Config msg =
    { label : String
    , value : String
    , options : List Option
    , onChange : String -> msg
    }


select :
    { label : String
    , value : String
    , onChange : String -> msg
    , options : List Option
    }
    -> Select msg
select config =
    Select
        { label = config.label
        , value = config.value
        , onChange = config.onChange
        , options = config.options
        }


option :
    { label : String
    , value : String
    }
    -> Option
option config =
    Option config


toHtml : Select msg -> Html msg
toHtml (Select config) =
    Html.label [ class "ui-select" ]
        [ Html.span [] [ text config.label ]
        , config.options
            |> List.map
                (\(Option { value, label }) ->
                    Html.option [ HA.value value ] [ text label ]
                )
            |> Html.select
                [ value config.value
                , onInput config.onChange
                ]
        ]
