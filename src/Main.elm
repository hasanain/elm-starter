module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onInput)
import Secondary


-- MODEL


main =
    Html.beginnerProgram { model = model, view = view, update = update }


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
view model =
    div []
        [ div []
            [ div []
                [ input [ type_ "text", value model.primary, onInput NewStringInput ] []
                ]
            , div [] [ model.primary |> String.reverse |> text ]
            ]
        ,  Html.map SecondaryMsg <|  Secondary.view model.secondary
        ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        NewStringInput newString ->
            { model | primary = newString }
        SecondaryMsg m -> 
            let 
                newSecondaryString = Secondary.update m model.secondary
            in
                {model | secondary = newSecondaryString } 
