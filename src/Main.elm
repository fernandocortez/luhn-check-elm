module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import LuhnCheck exposing (luhnCheck)
import Util exposing (formatPrimaryAccountNumber)


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = model, view = view, update = update }



-- MODEL


type alias Model =
    { primaryAccountNumber : String
    , isValid : Bool
    }


model : Model
model =
    { primaryAccountNumber = ""
    , isValid = False
    }



-- UPDATE


type Msg
    = UpdateCreditCardNumber String


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateCreditCardNumber value ->
            let
                pan =
                    formatPrimaryAccountNumber value
            in
                { model | primaryAccountNumber = pan, isValid = luhnCheck pan }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ label [ for "primaryAccountNumber" ] [ text "Credit Card Number" ]
            , input
                [ type_ "tel"
                , id "primaryAccountNumber"
                , onInput UpdateCreditCardNumber
                , autofocus True
                , maxlength 16
                ]
                []
            ]
        , strong []
            [ text (toString model.isValid)
            ]
        ]
