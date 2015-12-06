module Slider (..) where

import Html exposing (Html)
import Html.Attributes as Html
import Html.Events as Html
import String


slider : String -> a -> a -> a -> a -> (String -> Signal.Message) -> Html
slider name min max step default action =
    Html.label
        []
        [ Html.text name
        , Html.input
            [ Html.type' "range"
            , Html.min <| toString min
            , Html.max <| toString max
            , Html.step <| toString step
            , Html.attribute "value" <| toString default
            , Html.on "input" Html.targetValue action
            ]
            []
        ]


int : String -> Int -> Int -> Int -> Int -> Signal.Mailbox Int -> Html
int name min max step default mbox =
    slider name min max step default (String.toInt >> Result.toMaybe >> Maybe.withDefault default >> Signal.message mbox.address)


float : String -> Float -> Float -> Float -> Float -> Signal.Mailbox Float -> Html
float name min max step default mbox =
    slider name min max step default (String.toFloat >> Result.toMaybe >> Maybe.withDefault default >> Signal.message mbox.address)
