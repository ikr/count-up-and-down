module Form exposing (initialModel)

import Date exposing (Date)


type alias Model =
    { day : Int, month : Date.Month, year : Int, hour : Int, minute : Int }


initialModel : Date -> Model
initialModel defaultValue =
    Model
        (Date.day defaultValue)
        (Date.month defaultValue)
        (Date.year defaultValue)
        (Date.hour defaultValue)
        (Date.minute defaultValue)
