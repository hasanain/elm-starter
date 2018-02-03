module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onInput)


-- MODEL


main =
    Html.beginnerProgram { model = model, view = view, update = update }


type alias Model =
    String


model : Model
model =
    ""


type Msg
    = NewStringInput String


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ input [ type_ "text", value model, onInput NewStringInput ] []
            ]
        , div [] [ model |> String.reverse |> text ]
        ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        NewStringInput newString ->
            newString
