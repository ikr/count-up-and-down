module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


main : Html msg
main =
    Html.form [] [ directionChoice, baseDateInput, controlButtons ]


directionChoice : Html msg
directionChoice =
    div []
        [ input [ type_ "radio", name "direction", id "directionUp" ] []
        , label [ for "directionUp" ] [ text "Count up from" ]
        , input [ type_ "radio", name "direction", id "directionDown" ] []
        , label [ for "directionDown" ] [ text "Count down to" ]
        ]


baseDateInput : Html msg
baseDateInput =
    input [ type_ "date", name "baseDate", placeholder "YYYY-MM-DD" ] []


controlButtons : Html msg
controlButtons =
    div []
        [ button [ type_ "button" ] [ text "Save" ]
        , button [ type_ "button" ] [ text "Cancel" ]
        ]
