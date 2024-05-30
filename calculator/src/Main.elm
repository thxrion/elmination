-- [-------------]
--
-- [+] [-] [*] [/]
-- [1] [2] [3] [C]
-- [4] [5] [6] [<]
-- [7] [8] [9] [.]
-- [0] [(] [)] [=]

-- todo add sound
module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Round

import Computer exposing (parse, evaluate)

main : Program () Model Msg
main =
    Browser.sandbox
        { init = "", update = update, view = view }

type alias Model = String

type Msg
    = Compute
    | Reset
    | Push String
    | PushWithSound String
    | Pop

update : Msg -> Model -> Model
update msg model =
  case msg of
    Compute ->
        case parse model of
            Ok expr ->
                Round.round 2 (evaluate expr)
            Err _ ->
                ""
    Reset ->
        ""
    Push x ->
        model ++ x
    PushWithSound x ->
        model ++ x
    Pop ->  
        String.dropRight 1 model

viewButton : String -> Html Msg
viewButton key =
    button
        [ class "keypad__button", onClick (Push key) ]
        [ text key ]

viewResetButton : Html Msg
viewResetButton =
    button
        [class "keypad__button", onClick Reset]
        [text "C" ]

viewBackButton : Html Msg
viewBackButton =
    button
        [class "keypad__button", onClick Pop]
        [text "BACK" ]

viewComputeButton : Html Msg
viewComputeButton =
    button
        [class "keypad__button", onClick Compute]
        [text "=" ]

view : Model -> Html Msg
view model =
  div [ class "calculator"]
    [ div [ class "display" ]
        [ p
            [ class "display__text" ]
            [ text model ]
        ]
    , div [ class "keypad" ]
        [ viewButton "+"
        , viewButton "-"
        , viewButton "*"
        , viewButton "/"
        , viewButton "1"
        , viewButton "2"
        , viewButton "3"
        , viewResetButton
        , viewButton "4"
        , viewButton "5"
        , viewButton "6"
        , viewBackButton
        , viewButton "7"
        , viewButton "8"
        , viewButton "9"
        , viewButton "."
        , viewButton "0"
        , viewButton "("
        , viewButton ")"
        , viewComputeButton
        ]
    ]
