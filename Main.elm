module Main (..) where

import Svg exposing (svg, path, g, text, text', rect)
import Svg.Attributes exposing (width, height, viewBox, fill, transform, d, x, y, rx, ry, style, textAnchor)
import Easing exposing (ease, easeOutElastic, float)
import Effects exposing (Effects)
import Html exposing (Html)
import Svg.Events exposing (onClick)
import Time exposing (Time, second)
import StartApp
import Task


-- MODEL


type alias Model =
    { angle : Float
    , animationState : AnimationState
    }


type alias AnimationState =
    { prevClockTime : Time
    , elapsedTime : Time
    }


rotateStep =
    90


duration =
    2 * second



-- UPDATE
-- VIEW


trans : ( ( Float, Float ), ( Float, Float, Float ) ) -> Svg.Attribute
trans ( t, r ) =
    let
        ( a, x, y ) = r
    in
        transform <| "translate" ++ (toString t) ++ ", rotate(" ++ (toString a) ++ " " ++ (toString x) ++ " " ++ (toString y) ++ ")"


trans2 : ( ( Float, Float ), Float ) -> Svg.Attribute
trans2 ( t, a ) =
    transform <| "translate" ++ (toString t) ++ ", rotate(" ++ (toString a) ++ ")"


view : Time -> Html
view now =
    let
        tr x y =
            let
                pulse =
                    ease (Easing.easeInOutQuad |> Easing.retour) float 1.0 1.1

                silence =
                    ease (Easing.linear) float 1.0 1.0

                first t0 a1 a2 d t =
                    if t <= t0 then
                        a1 t0 t
                    else
                        a2 (d - t0) (t - t0)

                anim =
                    Easing.cycle <| first (0.7) silence pulse

                s =
                    anim duration (now - (y / 280 * 800))
            in
                transform <| "scale(" ++ (toString s) ++ ")"
    in
        svg
            [ width "560", height "560" ]
            [ g
                [ width "280", height "280", transform "translate(140,140)" ]
                [ path [ tr 104 33, fill "#7ed13b", d "M-132.3 -140h133c20.3 20.4 40.7 40.7 61 61-44 .1-88 0-133 0-20.5-20.3-40.8-40.8-61-61" ] []
                , path [ tr 244 42, fill "#5fb4cb", d "M16 -140h124v124c-41-41-83-83-124-124" ] []
                , path [ tr 50 140, fill "#596378", d "M-140 -132.3c44 44 88 88 133 133-44 44-88 88-133 133v-265" ] []
                , path [ tr 140 97, fill "#f0ad00", d "M-60 -68c40-.1 81-.1 121 0-20.3 20-40 40-61 61-20-20-40-40-60-61" ] []
                , path [ tr 214 140, fill "#7ed13b", d "M74 -66c22 21.8 43.8 43.7 66 66v1.1c-22 21.9-43.8 43.9-65.9 65.8-22-22-44-44-66-66 22-22 44-44 66-66" ] []
                , path [ tr 140 228, fill "#5fb4cb", d "M-131.4 140c43.9-43.9 88-88 132-132 44 43.8 88 88 132 132h-264" ] []
                , path [ tr 259 214, fill "#f0ad00", d "M82 74c19-19.7 38.8-38.8 58-58v116c-19.4-19.3-38.6-38.8-58-58" ] []
                ]
            ]


main =
    Signal.map view (Time.every 10)
