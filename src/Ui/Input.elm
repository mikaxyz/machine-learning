module Ui.Input exposing
    ( input
    , toHtml
    , withNumber
    , withNumberRange
    , withNumberRangeAndStep
    )

import Html exposing (..)
import Html.Attributes as HA exposing (..)
import Html.Events exposing (onInput)


type Input msg
    = Input (Config msg)


type Type
    = Text
    | Number
    | NumberRange Int Int
    | NumberRangeAndStep Int Int Float


type alias Config msg =
    { type_ : Type
    , placeholder : Maybe String
    , label : String
    , value : String
    , onChange : String -> msg
    }


input :
    { label : String
    , value : String
    , onChange : String -> msg
    }
    -> Input msg
input config =
    Input
        { type_ = Text
        , placeholder = Nothing
        , label = config.label
        , value = config.value
        , onChange = config.onChange
        }


withNumber : Input msg -> Input msg
withNumber (Input config) =
    Input { config | type_ = Number }


withNumberRange : Int -> Int -> Input msg -> Input msg
withNumberRange min_ max_ (Input config) =
    Input
        { config
            | type_ = NumberRange min_ max_
        }


withNumberRangeAndStep : Int -> Int -> Float -> Input msg -> Input msg
withNumberRangeAndStep min_ max_ step (Input config) =
    Input
        { config
            | type_ = NumberRangeAndStep min_ max_ step
        }


toHtml : Input msg -> Html msg
toHtml (Input config) =
    Html.label [ class "ui-input" ]
        [ Html.span [] [ text config.label ]
        , Html.input
            ([ value config.value
             , onInput config.onChange
             ]
                |> addTypeAttributes config.type_
            )
            []
        ]


addTypeAttributes : Type -> List (Attribute msg) -> List (Attribute msg)
addTypeAttributes inputType attr =
    case inputType of
        Text ->
            HA.type_ "text" :: attr

        Number ->
            HA.type_ "number" :: attr

        NumberRange minValue maxValue ->
            HA.type_ "number"
                :: HA.min (String.fromInt minValue)
                :: HA.max (String.fromInt maxValue)
                :: attr

        NumberRangeAndStep minValue maxValue step ->
            HA.type_ "number"
                :: HA.min (String.fromInt minValue)
                :: HA.max (String.fromInt maxValue)
                :: HA.step (String.fromFloat step)
                :: attr
