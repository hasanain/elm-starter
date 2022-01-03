module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onInput)
import Secondary



-- MODEL


main : Program () Model Msg
main =
    Browser.sandbox { init = model, update = update, view = view }


type alias Model =
    { primary : String
    , secondary : String
    }


model : Model
model =
    { primary = ""
    , secondary = ""
    }


type Msg
    = NewStringInput String
    | SecondaryMsg Secondary.Msg


view : Model -> Html Msg
view m =
    div []
        [ div []
            [ div []
                [ input [ type_ "text", value m.primary, onInput NewStringInput ] []
                ]
            , div [] [ m.primary |> String.reverse |> text ]
            ]
        , Html.map SecondaryMsg <| Secondary.view m.secondary
        ]


update : Msg -> Model -> Model
update msg m =
    case msg of
        NewStringInput newString ->
            { m | primary = newString }

        SecondaryMsg sMsg ->
            let
                newSecondaryString =
                    Secondary.update sMsg m.secondary
            in
            { m | secondary = newSecondaryString }
