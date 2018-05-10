module Secondary exposing (Msg, view, update)

import Html exposing (..)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onInput)


type SecondaryMsg
    = SecondaryInput String


type Msg
    = NewStringInput String


view : String -> Html Msg
view model =
    div []
        [ div []
            [ input [ type_ "text", onInput NewStringInput ] []
            ]
        , div [] [ model |> text ]
        ]


update : Msg -> String -> String
update msg model =
    case msg of
        NewStringInput newString ->
            newString