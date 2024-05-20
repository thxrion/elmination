module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)

main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }

type alias Model = String

init : Model
init = ""

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
        model
    Reset ->
        ""
    Push x ->
        model ++ x
    PushWithSound x ->
        model ++ x
    Pop ->  
        String.dropRight 1 model

viewSimpleButton : String -> Html Msg
viewSimpleButton key =
    button
        [ class "keypad__regular"
        , onClick (Push key)
        ]
        [ text key ]

view : Model -> Html Msg
view model =
  div [ class "calculator"]
    [ div [ class "display" ]
        [ p [ class "display__text" ] [ text model ]
        ]
    , div [ class "keypad"]
        [ viewSimpleButton "+"
        , viewSimpleButton "-"
        , viewSimpleButton "*"
        , viewSimpleButton "/"

        , viewSimpleButton "1"
        , viewSimpleButton "2"
        , viewSimpleButton "3"
        , button
            [ class "keypad__regular"
            , onClick Reset
            ]
            [ text "C" ]

        , viewSimpleButton "4"
        , viewSimpleButton "5"
        , viewSimpleButton "6"
        , button
            [ class "keypad__regular"
            , onClick Pop
            ]
            [ text "BACK" ]

        , viewSimpleButton "7"
        , viewSimpleButton "8"
        , viewSimpleButton "9"

        , button
            [ class "keypad__equals"
            , onClick Compute
            ]
            [ text "=" ]

        , button
            [ class "keypad__zero"
            , onClick (Push "0")
            ]
            [ text "0" ]
        , viewSimpleButton "."
        ]
    ]


-- [-------------]
--
-- [+] [-] [*] [/]
-- [1] [2] [3] [C]
-- [4] [5] [6] [<]
-- [7] [8] [9] [=]
-- [  0  ] [.] [ ]
