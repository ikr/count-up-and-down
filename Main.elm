module Main exposing (..)

import Date exposing (Date)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Task
import Time exposing (Time, second)


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


type Mode
    = Tick Counting
    | Edit (Maybe Counting) Form


type alias Model =
    { mode : Mode, now : Date }


type Msg
    = FormChangeDirection DirectionField
    | FormChangeDate DateField
    | FormSubmit
    | FormCancel
    | CurrentTime Date



-- MODEL


main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : ( Model, Cmd Msg )
init =
    ( model, Task.perform CurrentTime Date.now )


model : Model
model =
    { mode =
        Edit Nothing
            { direction = DirectionUndefined
            , date = DateUndefined
            , error = Nothing
            }
    , now = Date.fromTime 0
    }


subscriptions : Model -> Sub Msg
subscriptions { mode } =
    case mode of
        Tick _ ->
            Time.every second (CurrentTime << Date.fromTime)

        Edit _ _ ->
            Sub.none



-- VIEW


view : Model -> Html Msg
view { mode, now } =
    case mode of
        Tick counting ->
            ticker now counting

        Edit countingOrNot { direction, date } ->
            Html.form [ onSubmit FormSubmit ]
                [ directionChoice direction
                , dateInput date
                , controlButtons countingOrNot
                ]


ticker : Date -> Counting -> Html msg
ticker now counting =
    let
        origin =
            case counting of
                UpFrom d ->
                    d

                DownTo d ->
                    d

        diff =
            abs <| Date.toTime origin - Date.toTime now
    in
        div [] [ text <| (toString diff) ++ " ms" ]


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


controlButtons : Maybe Counting -> Html Msg
controlButtons countingOrNot =
    div []
        (button [ type_ "submit" ] [ text "Save" ]
            :: case countingOrNot of
                Just _ ->
                    [ button [ type_ "button", onClick FormCancel ] [ text "Cancel" ] ]

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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg { mode, now } =
    case mode of
        Tick _ ->
            case msg of
                CurrentTime t ->
                    ( { mode = mode, now = t }, Cmd.none )

                _ ->
                    ( Model mode now, Cmd.none )

        Edit countingOrNot form ->
            case msg of
                FormChangeDirection directionField ->
                    ( Model
                        (Edit countingOrNot { form | direction = directionField })
                        now
                    , Cmd.none
                    )

                FormChangeDate dateField ->
                    ( Model
                        (Edit countingOrNot { form | date = dateField, error = Nothing })
                        now
                    , Cmd.none
                    )

                FormSubmit ->
                    let
                        { direction, date } =
                            form
                    in
                        case ( direction, date ) of
                            ( DirectionUp, DateValid d ) ->
                                ( Model (Tick (UpFrom d)) now, Cmd.none )

                            ( DirectionDown, DateValid d ) ->
                                ( Model (Tick (DownTo d)) now, Cmd.none )

                            ( _, _ ) ->
                                ( Model
                                    (Edit
                                        countingOrNot
                                        { form | error = Just "This isn't a valid date" }
                                    )
                                    now
                                , Cmd.none
                                )

                FormCancel ->
                    case countingOrNot of
                        Just counting ->
                            ( Model (Tick counting) now, Cmd.none )

                        Nothing ->
                            ( Model (Edit Nothing form) now, Cmd.none )

                CurrentTime _ ->
                    ( Model mode now, Cmd.none )
