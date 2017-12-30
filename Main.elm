module Main exposing (..)

import Date exposing (Date)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Task
import Time exposing (Time, second)


type DateField
    = DateUndefined
    | DateInvalid
    | DateValid Date


type alias Form =
    { date : DateField, error : Maybe String }


type Mode
    = Tick Date
    | Edit (Maybe Date) Form


type alias Model =
    { mode : Mode, now : Date }


type Msg
    = FormChangeDate DateField
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
            { date = DateUndefined
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
        Tick origin ->
            ticker now origin

        Edit originOrNot form ->
            formContainer originOrNot form


ticker : Date -> Date -> Html msg
ticker dateA dateB =
    div [] [ text <| tickerString dateA dateB ]


tickerString : Date -> Date -> String
tickerString dateA dateB =
    toString <| abs <| Date.toTime dateA - Date.toTime dateB


formContainer : Maybe Date -> Form -> Html Msg
formContainer originOrNot { date, error } =
    div []
        ([ form originOrNot date ]
            ++ case error of
                Just errorString ->
                    [ text errorString ]

                Nothing ->
                    []
        )


form : Maybe Date -> DateField -> Html Msg
form originOrNot dateField =
    Html.form [ onSubmit FormSubmit ]
        [ dateLabel
        , dateInput dateField
        , controlButtons originOrNot
        ]


dateLabel : Html Msg
dateLabel =
    label [ for "date" ] [ text "Count up-from/down-to" ]


dateInput : DateField -> Html Msg
dateInput dateField =
    input
        [ type_ "date"
        , id "date"
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


controlButtons : Maybe Date -> Html Msg
controlButtons originOrNot =
    div []
        (button [ type_ "submit" ] [ text "Save" ]
            :: case originOrNot of
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

        Edit originOrNot form ->
            case msg of
                FormChangeDate dateField ->
                    ( Model
                        (Edit originOrNot { form | date = dateField, error = Nothing })
                        now
                    , Cmd.none
                    )

                FormSubmit ->
                    let
                        { date } =
                            form
                    in
                        case date of
                            DateValid d ->
                                ( Model (Tick d) now, Cmd.none )

                            _ ->
                                ( Model
                                    (Edit
                                        originOrNot
                                        { form | error = Just "This isn't a valid date" }
                                    )
                                    now
                                , Cmd.none
                                )

                FormCancel ->
                    case originOrNot of
                        Just origin ->
                            ( Model (Tick origin) now, Cmd.none )

                        Nothing ->
                            ( Model (Edit Nothing form) now, Cmd.none )

                CurrentTime t ->
                    ( { mode = mode, now = t }, Cmd.none )
