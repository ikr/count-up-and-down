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
    = OriginDefined OriginDefined
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
            Html.form [] [ directionChoice, dateInput, controlButtons origin ]


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


dateInput : Html msg
dateInput =
    input
        [ type_ "date"
        , name "date"
        , required True
        , placeholder "YYYY-MM-DD"
        , title "YYYY-MM-DD"
        , pattern "^\\d{4}-[0-1]\\d-[0-3]\\d$"
        ]
        []


dateInputDefaultValue : Origin -> String
dateInputDefaultValue origin =
    case origin of
        OriginUndefined ->
            ""

        OriginDefined (Up date) ->
            iso8601 date

        OriginDefined (Down date) ->
            iso8601 date


controlButtons : Origin -> Html msg
controlButtons origin =
    div []
        (button [ type_ "submit" ] [ text "Save" ]
            :: case origin of
                OriginDefined _ ->
                    [ button [ type_ "button" ] [ text "Cancel" ] ]

                OriginUndefined ->
                    []
        )


iso8601 : Date -> String
iso8601 date =
    String.join "-"
        [ Date.year date |> toString
        , case Date.month date of
            Date.Jan ->
                "01"

            Date.Feb ->
                "02"

            Date.Mar ->
                "03"

            Date.Apr ->
                "04"

            Date.May ->
                "05"

            Date.Jun ->
                "06"

            Date.Jul ->
                "07"

            Date.Aug ->
                "08"

            Date.Sep ->
                "09"

            Date.Oct ->
                "10"

            Date.Nov ->
                "11"

            Date.Dec ->
                "12"
        , Date.day date |> toString |> String.padLeft 2 '0'
        ]
