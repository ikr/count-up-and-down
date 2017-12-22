module Main exposing (..)

import Date exposing (Date)
import Html exposing (..)
import Html.Attributes exposing (..)


type DirectionField
    = DirectionUndefined
    | DirectionUp
    | DirectionDown


type DateField
    = DateUndefined
    | DateFieldError String
    | Date


type alias Form =
    { direction : DirectionField, date : DateField }


type OriginDefined
    = Up Date
    | Down Date


type Origin
    = OriginDefined
    | OriginUndefined


type Model
    = Tick OriginDefined
    | Edit Origin Form


model : Model
model =
    Edit OriginUndefined { direction = DirectionUndefined, date = DateUndefined }


main : Html msg
main =
    view model


view : Model -> Html msg
view model =
    case model of
        Tick _ ->
            ticker

        Edit origin _ ->
            Html.form [] [ directionChoice, baseDateInput, controlButtons origin ]


ticker : Html msg
ticker =
    div [] [ text "Not implemented yet" ]


directionChoice : Html msg
directionChoice =
    div []
        [ input
            [ type_ "radio"
            , name "direction"
            , value "up"
            , id "directionUp"
            , required True
            ]
            []
        , label [ for "directionUp" ] [ text "Count up from" ]
        , input
            [ type_ "radio"
            , name "direction"
            , value "down"
            , id "directionDown"
            , required True
            ]
            []
        , label [ for "directionDown" ] [ text "Count down to" ]
        ]


baseDateInput : Html msg
baseDateInput =
    input
        [ type_ "date"
        , name "baseDate"
        , required True
        , placeholder "YYYY-MM-DD"
        , title "YYYY-MM-DD"
        , pattern "^\\d{4}-[0-1]\\d-[0-3]\\d$"
        ]
        []


controlButtons : Origin -> Html msg
controlButtons origin =
    div []
        (button [ type_ "submit" ] [ text "Save" ]
            :: case origin of
                OriginDefined ->
                    [ button [ type_ "button" ] [ text "Cancel" ] ]

                OriginUndefined ->
                    []
        )
