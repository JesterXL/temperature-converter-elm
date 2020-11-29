module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, h2, input, label, section, text)
import Html.Attributes exposing (class, name, type_, for, value)
import Html.Events exposing (onInput)
import Debug
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Decimals(..), Locale, usLocale)

type alias Model =
    { fahrenheitFieldValid : Bool
    , celsiusFieldValid : Bool
    , fahrenheit : Float 
    , celsius : Float
    , fahrenheitFieldValue : String
    , celsiusFieldValue : String }


initialModel : Model
initialModel =
    { fahrenheitFieldValid = True
    , celsiusFieldValid = True
    , fahrenheit = 32
    , celsius = 0 
    , fahrenheitFieldValue = "32"
    , celsiusFieldValue = "0" }


type Msg
    = FahrenheitToCelsius String
    | CelsiusToFahrenheit String


update : Msg -> Model -> Model
update msg model =
    case msg of
        FahrenheitToCelsius userInput ->
            case String.toFloat userInput of
                Nothing ->
                    { model
                        | fahrenheitFieldValue = userInput
                        , fahrenheitFieldValid = False }
                Just number ->
                    { model 
                        | fahrenheit = number
                        , celsius = fahrenheitToCelsius number
                        , fahrenheitFieldValue = userInput
                        , fahrenheitFieldValid = True
                        , celsiusFieldValid = True }
            
        CelsiusToFahrenheit userInput ->
            case String.toFloat userInput of
                Nothing ->
                    { model
                        | celsiusFieldValid = False
                        , celsiusFieldValue = userInput }
                Just number ->
                    { model
                        | celsius = number
                        , fahrenheit = celsiusToFahrenheit number
                        , celsiusFieldValue = userInput
                        , celsiusFieldValid = True 
                        , fahrenheitFieldValid = True }
                    
fahrenheitToCelsius fahrenheit =
    (fahrenheit - 32) / 1.8
    
celsiusToFahrenheit celsius =
    (celsius * 1.8) + 32
        

view : Model -> Html Msg
view model =
    section [ class "section" ]
        [ div [ class "container" ]
            [ h1 [ class "title" ] [ text "Temperature Converter" ]
            , h2 [ class "subtitle" ] [ text "Converting from Fahrenheit to Celsius. Just start typing." ]
            , label [ for "fahrenheit", class "label" ] [text "Fahrenheit:"]
            , input
                (
                  [ name "fahrenheit"
                  , type_ "text"
                  , onInput FahrenheitToCelsius
                  , value (viewFahrenheit model)
                  ]
                  ++
                  getFahrenheitFieldValidOrNot model
                )
                []
            , label [ for "celsius", class "label" ] [text "Celsius:"]
            , input
                (
                  [ name "fahrenheit"
                  , type_ "text"
                  , onInput CelsiusToFahrenheit
                  , value (viewCelsius model)
                  ]
                  ++
                  getCelsiusFieldValidOrNot model
                )
                []
            ]
        ]

getFahrenheitFieldValidOrNot model =
    if model.fahrenheitFieldValid == True then
        [class "input"]
    else
        [class "input is-danger"]
        
getCelsiusFieldValidOrNot model =
    if model.celsiusFieldValid == True then
        [class "input"]
    else
        [class "input is-danger"]


viewFahrenheit model =
    if model.fahrenheitFieldValid == True then
        model.fahrenheit |> formatTemperature
    else
        model.fahrenheitFieldValue
        
viewCelsius model =
    if model.celsiusFieldValid == True then
        model.celsius |> formatTemperature
    else
        model.celsiusFieldValue

formatTemperature number =
    format { usLocale | decimals = Max 1 } number

main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
