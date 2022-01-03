module Secondary exposing (Msg, update, view)

import Html exposing (..)
import Html.Attributes exposing (type_)
import Html.Events exposing (onInput)


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
update msg _ =
    case msg of
        NewStringInput newString ->
            newString
