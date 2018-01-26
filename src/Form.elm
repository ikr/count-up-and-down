module Form exposing (initialModel, Model)

import Date exposing (Date)


-- MODEL


type alias Model =
    ( Fields, Error )


type alias Fields =
    { day : Int, month : Date.Month, year : Int, hour : Int, minute : Int }


type alias Error =
    Maybe String


type Msg
    = Change Fields
    | Submit
    | Cancel


initialModel : Date -> Model
initialModel defaultValue =
    ( Fields
        (Date.day defaultValue)
        (Date.month defaultValue)
        (Date.year defaultValue)
        (Date.hour defaultValue)
        (Date.minute defaultValue)
    , Nothing
    )



-- VIEW
