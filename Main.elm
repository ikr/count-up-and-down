module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


main : Html msg
main =
    Html.form [] [ directionChoice, baseDateInput, controlButtons ]


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


controlButtons : Html msg
controlButtons =
    div []
        [ button [ type_ "submit" ] [ text "Save" ]
        , button [ type_ "button" ] [ text "Cancel" ]
        ]
