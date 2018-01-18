port module Main exposing (..)

import Date exposing (Date)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Task
import Time exposing (Time, second)
import Date.Extra.Create exposing (dateFromFields)
import Date.Extra.Duration exposing (diff, DeltaRecord)
import Date.Extra.Format exposing (format, isoString)
import Date.Extra.Config.Config_de_de exposing (config)


type DateField
    = DateUndefined
    | DateInvalid
    | DateValid Date


type alias Form =
    { dateField : DateField, timeField : DateField, error : Maybe String }


type Mode
    = Tick Date
    | Edit (Maybe Date) Form


type alias Model =
    { mode : Mode, now : Date }


type Msg
    = FormChangeDate DateField
    | FormChangeTime DateField
    | FormSubmit
    | FormCancel
    | CurrentTime Date
    | SwitchToEdit


type alias Flags =
    { origin : Maybe String }



-- BOOTSTRAP


main : Program Flags Model Msg
main =
    programWithFlags
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : Flags -> ( Model, Cmd Msg )
init { origin } =
    let
        mode =
            case origin of
                Nothing ->
                    Edit Nothing
                        { dateField = DateUndefined
                        , timeField = DateUndefined
                        , error = Nothing
                        }

                Just dateString ->
                    initMode dateString
    in
        ( { mode = mode, now = Date.fromTime 0 }
        , Task.perform CurrentTime Date.now
        )


initMode : String -> Mode
initMode dateString =
    case Date.fromString dateString of
        Err error ->
            Edit Nothing
                { dateField = DateUndefined
                , timeField = DateUndefined
                , error =
                    Just <|
                        "Failed reading the persisted origin date: "
                            ++ error
                }

        Ok date ->
            Tick date


subscriptions : Model -> Sub Msg
subscriptions { mode } =
    case mode of
        Tick _ ->
            Time.every second (CurrentTime << Date.fromTime)

        Edit _ _ ->
            Sub.none


port persist : String -> Cmd msg



-- VIEW


view : Model -> Html Msg
view { mode, now } =
    case mode of
        Tick origin ->
            ticker now origin

        Edit originOrNot form ->
            formContainer originOrNot form


ticker : Date -> Date -> Html Msg
ticker dateA dateB =
    div [ class "jumbotron" ]
        [ h1
            [ class "ticker-string display-4"
            , onClick SwitchToEdit
            ]
            [ text <| tickerString dateA dateB ]
        ]


tickerString : Date -> Date -> String
tickerString dateA dateB =
    let
        d =
            diff dateB dateA
    in
        yearsMonthsDaysString d ++ " " ++ hoursMinutesSecondsString d


hoursMinutesSecondsString : DeltaRecord -> String
hoursMinutesSecondsString d =
    String.join ":" <|
        List.map (String.padLeft 2 '0' << toString) [ d.hour, d.minute, d.second ]


yearsMonthsDaysString : DeltaRecord -> String
yearsMonthsDaysString d =
    List.map
        pluralize
        [ ( "year", "years", d.year ), ( "month", "months", d.month ), ( "day", "days", d.day ) ]
        |> String.join " "


pluralize : ( String, String, number ) -> String
pluralize ( single, plural, count ) =
    if count == 1 then
        toString count ++ " " ++ single
    else
        toString count ++ " " ++ plural


formContainer : Maybe Date -> Form -> Html Msg
formContainer originOrNot { dateField, error } =
    div []
        ([ form originOrNot dateField ]
            ++ case error of
                Just errorString ->
                    [ errorElement errorString ]

                Nothing ->
                    []
        )


form : Maybe Date -> DateField -> Html Msg
form originOrNot dateField =
    Html.form [ onSubmit FormSubmit ]
        [ div [ class "form-group" ]
            [ dateLabel
            , dateInput dateField
            ]
        , div [ class "form-group" ]
            [ timeInput dateField ]
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
        , class "form-control"
        ]
        []


timeInput : DateField -> Html Msg
timeInput timeField =
    input
        [ type_ "time"
        , id "time"
        , name "time"
        , required True
        , placeholder "HH:MM"
        , title "HH:MM"
        , pattern "^[0-2]\\d:[0-5]\\d$"
        , defaultValue <| timeInputDefaultValue timeField
        , onInput timeInputOnInputHandler
        , class "form-control"
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
            format config "%Y-%m-%d" date


timeInputDefaultValue : DateField -> String
timeInputDefaultValue dateField =
    case dateField of
        DateUndefined ->
            ""

        DateInvalid ->
            ""

        DateValid date ->
            format config "%H:%M" date


dateInputOnInputHandler : String -> Msg
dateInputOnInputHandler value =
    case Date.fromString value of
        Ok d ->
            FormChangeDate
                (DateValid <| dateFromFields (Date.year d) (Date.month d) (Date.day d) 0 0 0 0)

        Err _ ->
            FormChangeDate DateInvalid


timeInputOnInputHandler : String -> Msg
timeInputOnInputHandler value =
    case Date.fromString <| "1970-01-01T" ++ value ++ ":00" of
        Ok d ->
            FormChangeTime
                (DateValid <| dateFromFields 1970 Date.Jan 1 (Date.hour d) (Date.minute d) 0 0)

        Err _ ->
            FormChangeTime DateInvalid


controlButtons : Maybe Date -> Html Msg
controlButtons originOrNot =
    div [ class "form-group" ]
        (button [ type_ "submit", class "btn btn-primary" ] [ text "Save" ]
            :: case originOrNot of
                Just _ ->
                    [ text " "
                    , button
                        [ type_ "button"
                        , class "btn btn-secondary"
                        , onClick FormCancel
                        ]
                        [ text "Cancel" ]
                    ]

                Nothing ->
                    []
        )


errorElement : String -> Html msg
errorElement errorMessage =
    div [ class "alert alert-warning" ] [ text errorMessage ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FormChangeDate dateField ->
            updateOnFormChangeDate dateField model

        FormChangeTime timeField ->
            updateOnFormChangeTime timeField model

        FormSubmit ->
            updateOnFormSubmit model

        FormCancel ->
            updateOnFormCancel model

        CurrentTime t ->
            updateOnCurrentTime t model

        SwitchToEdit ->
            updateOnSwitchToEdit model


updateOnFormChangeDate : DateField -> Model -> ( Model, Cmd Msg )
updateOnFormChangeDate dateField model =
    let
        { mode, now } =
            model
    in
        case mode of
            Edit originOrNot form ->
                ( Model
                    (Edit originOrNot { form | dateField = dateField, error = Nothing })
                    now
                , Cmd.none
                )

            Tick _ ->
                ( model, Cmd.none )


updateOnFormChangeTime : DateField -> Model -> ( Model, Cmd Msg )
updateOnFormChangeTime timeField model =
    let
        { mode, now } =
            model
    in
        case mode of
            Edit originOrNot form ->
                ( Model
                    (Edit originOrNot { form | timeField = timeField, error = Nothing })
                    now
                , Cmd.none
                )

            Tick _ ->
                ( model, Cmd.none )


updateOnFormSubmit : Model -> ( Model, Cmd Msg )
updateOnFormSubmit model =
    let
        { mode, now } =
            model
    in
        case mode of
            Edit originOrNot form ->
                let
                    { dateField, timeField } =
                        form
                in
                    case ( dateField, timeField ) of
                        ( DateValid d, DateValid t ) ->
                            let
                                newOrigin =
                                    dateFromFields (Date.year d)
                                        (Date.month d)
                                        (Date.day d)
                                        (Date.hour t)
                                        (Date.minute t)
                                        0
                                        0
                            in
                                ( Model (Tick newOrigin) now, persist <| isoString newOrigin )

                        ( DateInvalid, _ ) ->
                            ( { mode =
                                    Edit originOrNot
                                        { form
                                            | error = Just "This isn't a valid date"
                                        }
                              , now = now
                              }
                            , Cmd.none
                            )

                        ( DateUndefined, _ ) ->
                            ( { mode =
                                    Edit originOrNot
                                        { form
                                            | error = Just "No date"
                                        }
                              , now = now
                              }
                            , Cmd.none
                            )

                        ( _, DateInvalid ) ->
                            ( { mode =
                                    Edit originOrNot
                                        { form
                                            | error = Just "This isn't a valid time"
                                        }
                              , now = now
                              }
                            , Cmd.none
                            )

                        ( _, DateUndefined ) ->
                            ( { mode =
                                    Edit originOrNot
                                        { form
                                            | error = Just "No time"
                                        }
                              , now = now
                              }
                            , Cmd.none
                            )

            Tick _ ->
                ( model, Cmd.none )


updateOnFormCancel : Model -> ( Model, Cmd Msg )
updateOnFormCancel model =
    let
        { mode, now } =
            model
    in
        case mode of
            Edit originOrNot form ->
                case originOrNot of
                    Just origin ->
                        ( Model (Tick origin) now, Cmd.none )

                    Nothing ->
                        ( Model (Edit Nothing form) now, Cmd.none )

            Tick _ ->
                ( model, Cmd.none )


updateOnCurrentTime : Date -> Model -> ( Model, Cmd Msg )
updateOnCurrentTime d model =
    ( { model | now = d }, Cmd.none )


updateOnSwitchToEdit : Model -> ( Model, Cmd Msg )
updateOnSwitchToEdit model =
    let
        { mode, now } =
            model
    in
        case mode of
            Edit _ _ ->
                ( model, Cmd.none )

            Tick origin ->
                ( Model
                    (Edit (Just origin)
                        { dateField = DateValid origin
                        , timeField = DateValid origin
                        , error = Nothing
                        }
                    )
                    now
                , Cmd.none
                )
