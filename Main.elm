module Main exposing (..)

import Date exposing (Date)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onCheck)


type DirectionField
    = DirectionUndefined
    | DirectionUp
    | DirectionDown


type DateField
    = DateUndefined
    | DateInvalid
    | DateValid Date


type alias Form =
    { direction : DirectionField, date : DateField, error : Maybe String }


type Counting
    = UpFrom Date
    | DownTo Date


type Model
    = Tick Counting
    | Edit (Maybe Counting) Form


type Msg
    = FormChangeDirection DirectionField
    | FormChangeDate DateField
    | FormSubmit
    | FormCancel


model : Model
model =
    Edit Nothing { direction = DirectionUndefined, date = DateUndefined, error = Nothing }


main : Program Never Model Msg
main =
    beginnerProgram
        { model = model
        , view = view
        , update = update
        }



-- VIEW


view : Model -> Html Msg
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


directionChoice : DirectionField -> Html Msg
directionChoice direction =
    div []
        [ input
            (directionRadioAttributes "up" directionRadioUpOnCheckHandler <| direction == DirectionUp)
            []
        , label [ for "directionUp" ] [ text "Count up from" ]
        , input
            (directionRadioAttributes "down" directionRadioDownOnCheckHandler <| direction == DirectionDown)
            []
        , label [ for "directionDown" ] [ text "Count down to" ]
        ]


directionRadioAttributes : String -> (Bool -> Msg) -> Bool -> List (Attribute Msg)
directionRadioAttributes key handler isChecked =
    [ type_ "radio"
    , name "direction"
    , value key
    , id key
    , required True
    , onCheck handler
    ]
        ++ (if isChecked then
                [ checked True ]
            else
                []
           )


directionRadioUpOnCheckHandler : Bool -> Msg
directionRadioUpOnCheckHandler checked =
    if checked then
        FormChangeDirection DirectionUp
    else
        FormChangeDirection DirectionUndefined


directionRadioDownOnCheckHandler : Bool -> Msg
directionRadioDownOnCheckHandler checked =
    if checked then
        FormChangeDirection DirectionDown
    else
        FormChangeDirection DirectionUndefined


dateInput : DateField -> Html Msg
dateInput dateField =
    input
        [ type_ "date"
        , name "date"
        , required True
        , placeholder "YYYY-MM-DD"
        , title "YYYY-MM-DD"
        , pattern "^\\d{4}-[0-1]\\d-[0-3]\\d$"
        , defaultValue <| dateInputDefaultValue dateField
        , onInput dateInputOnInputHandler
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


dateInputOnInputHandler : String -> Msg
dateInputOnInputHandler value =
    case Date.fromString value of
        Ok date ->
            FormChangeDate (DateValid date)

        Err _ ->
            FormChangeDate DateInvalid


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



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case model of
        Tick _ ->
            model

        Edit countingOrNot form ->
            case msg of
                FormChangeDirection directionField ->
                    Edit countingOrNot { form | direction = directionField }

                FormChangeDate dateField ->
                    Edit countingOrNot { form | date = dateField, error = Nothing }

                FormSubmit ->
                    model

                FormCancel ->
                    model
