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
    | DateInvalid
    | DateValid Date


type alias Form =
    { direction : DirectionField, date : DateField }


type Counting
    = UpFrom Date
    | DownTo Date


type Model
    = Tick Counting
    | Edit (Maybe Counting) Form


model : Model
model =
    Edit Nothing { direction = DirectionUndefined, date = DateUndefined }


main : Html msg
main =
    view model


view : Model -> Html msg
view model =
    case model of
        Tick _ ->
            ticker

        Edit countingOrNot { direction, date } ->
            Html.form []
                [ directionChoice direction
                , dateInput date
                , controlButtons countingOrNot
                ]


ticker : Html msg
ticker =
    div [] [ text "Not implemented yet" ]


directionChoice : DirectionField -> Html msg
directionChoice direction =
    div []
        [ input
            (directionRadioAttributes "up" <| direction == DirectionUp)
            []
        , label [ for "directionUp" ] [ text "Count up from" ]
        , input
            (directionRadioAttributes "down" <| direction == DirectionDown)
            []
        , label [ for "directionDown" ] [ text "Count down to" ]
        ]


directionRadioAttributes : String -> Bool -> List (Attribute msg)
directionRadioAttributes key isChecked =
    [ type_ "radio"
    , name "direction"
    , value key
    , id key
    , required True
    ]
        ++ (if isChecked then
                [ checked True ]
            else
                []
           )


dateInput : DateField -> Html msg
dateInput dateField =
    input
        [ type_ "date"
        , name "date"
        , required True
        , placeholder "YYYY-MM-DD"
        , title "YYYY-MM-DD"
        , pattern "^\\d{4}-[0-1]\\d-[0-3]\\d$"
        , defaultValue <| dateInputDefaultValue dateField
        ]
        []


dateInputDefaultValue : DateField -> String
dateInputDefaultValue dateField =
    case dateField of
        DateUndefined ->
            ""

        DateInvalid ->
            ""

        DateValid date ->
            iso8601 date


controlButtons : Maybe Counting -> Html msg
controlButtons countingOrNot =
    div []
        (button [ type_ "submit" ] [ text "Save" ]
            :: case countingOrNot of
                Just _ ->
                    [ button [ type_ "button" ] [ text "Cancel" ] ]

                Nothing ->
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
