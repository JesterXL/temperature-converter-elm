module Main exposing (main)

import Browser
-- import Debug
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Decimals(..), Locale, usLocale)
import Html exposing (Html, button, div, form, h3, input, label, option, p, section, select, text, h1, h2, a, br)
import Html.Attributes exposing (action, class, for, id, method, name, type_, value, href)
import Html.Events exposing (onClick, onInput)


type alias Model =
    { fahrenheit : Float
    , celsius : Float
    , fahrenheitField : String
    , celsiusField : String
    , fahrenheitLegit : Bool
    , celsiusLegit : Bool
    }


initialModel : Model
initialModel =
    { fahrenheit = 32
    , celsius = 0
    , fahrenheitField = "32"
    , celsiusField = "0"
    , fahrenheitLegit = True
    , celsiusLegit = True
    }


type Msg
    = FahrenheitToCelsius String
    | CelsiusToFahrenheit String


update : Msg -> Model -> Model
update msg model =
    -- let
    --     msg1 =
    --         Debug.log "msg" msg
    -- in
    case msg of
        FahrenheitToCelsius userInput ->
            case String.toFloat userInput of
                Nothing ->
                    { model | fahrenheitField = userInput, fahrenheitLegit = False }

                Just number ->
                    { model | celsius = fToC number, fahrenheit = number, fahrenheitField = userInput, fahrenheitLegit = True, celsiusLegit = True }

        CelsiusToFahrenheit userInput ->
            case String.toFloat userInput of
                Nothing ->
                    { model | celsiusField = userInput, celsiusLegit = False }

                Just number ->
                    { model | fahrenheit = cToF number, celsius = number, celsiusField = userInput, fahrenheitLegit = True, celsiusLegit = True }


fToC f =
    (f - 32) / 1.8


cToF c =
    (c * 1.8) + 32


view : Model -> Html Msg
view model =
    section [ class "section" ]
        [ div [ class "container" ]
            [ h1 [class "title"][text "Temperture Converter"]
            , h2 [class "subtitle"][
                p [][text "Converts Fahrenheit to Celsius and Celsius to Fahrenheit. Type in numbers in either field and it'll convert the other one for you automatically."]
                , br [][]
                , p[][
                    text "Code: "
                    , a [href "https://github.com/JesterXL/temperature-converter-elm"][text "https://github.com/JesterXL/temperature-converter-elm"]
                ]
            ]
            , label [ for "fahrenheit", class "label" ] [ text "Fahrenheit" ]
            , input
                [ name "fahrenheit"
                , class "input"
                , type_ "text"
                , value (viewFahrenheit model)
                , onInput FahrenheitToCelsius
                ]
                []
            , label [ for "celsius", class "label" ] [ text "Celsius" ]
            , input
                [ name "celsius"
                , class "input"
                , type_ "text"
                , value (viewCelsius model)
                , onInput CelsiusToFahrenheit
                ]
                []
            ]
        ]


viewFahrenheit model =
    if model.fahrenheitLegit == True then
        formatTemperature model.fahrenheit

    else
        model.fahrenheitField


viewCelsius model =
    if model.celsiusLegit == True then
        formatTemperature model.celsius

    else
        model.celsiusField


formatTemperature float =
    format { usLocale | decimals = Max 2 } float


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
