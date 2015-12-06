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
    Maybe { prevClockTime : Time, elapsedTime : Time }


init : ( Model, Effects Action )
init =
    ( { angle = 0, animationState = Nothing }
    , Effects.none
    )


rotateStep =
    90


duration =
    1 * second



-- UPDATE


type Action
    = Spin
    | Tick Time


update : Action -> Model -> ( Model, Effects Action )
update msg model =
    case msg of
        Spin ->
            case model.animationState of
                Nothing ->
                    ( model, Effects.tick Tick )

                Just _ ->
                    ( model, Effects.none )

        Tick clockTime ->
            let
                newElapsedTime =
                    case model.animationState of
                        Nothing ->
                            0

                        Just { elapsedTime, prevClockTime } ->
                            elapsedTime + (clockTime - prevClockTime)
            in
                if newElapsedTime > duration then
                    ( { angle = model.angle + rotateStep
                      , animationState = Nothing
                      }
                    , Effects.none
                    )
                else
                    ( { angle = model.angle
                      , animationState = Just { elapsedTime = newElapsedTime, prevClockTime = clockTime }
                      }
                    , Effects.tick Tick
                    )



-- VIEW


toOffset : AnimationState -> Float
toOffset animationState =
    case animationState of
        Nothing ->
            0

        Just { elapsedTime } ->
            ease easeOutElastic float 0 rotateStep duration elapsedTime


trans : ( ( Float, Float ), ( Float, Float, Float ) ) -> Svg.Attribute
trans ( t, r ) =
    let
        ( a, x, y ) = r
    in
        transform <| "translate" ++ (toString t) ++ ", rotate(" ++ (toString a) ++ " " ++ (toString x) ++ " " ++ (toString y) ++ ")"


trans2 : ( ( Float, Float ), Float ) -> Svg.Attribute
trans2 ( t, a ) =
    transform <| "translate" ++ (toString t) ++ ", rotate(" ++ (toString a) ++ ")"


view : Signal.Address Action -> Model -> Html
view address model =
    let
        ( angle, dist ) =
            case model.animationState of
                Nothing ->
                    ( 0, 100 )

                Just { elapsedTime } ->
                    ( ease easeOutElastic float 0 rotateStep duration elapsedTime
                    , ease easeOutElastic float 100 200 duration elapsedTime
                    )

        t1 = trans ( ( 100, 100 ), ( model.angle + angle, 140, 140 ) )
    in
        svg
            [ width "560", height "560" ]
            [ g
                [ width "280", height "280", onClick (Signal.message address Spin) ]
                [ path [ t1, fill "#7ed13b", d "m7.7 0h133c20.3 20.4 40.7 40.7 61 61-44 .1-88 0-133 0-20.5-20.3-40.8-40.8-61-61" ] []
                , path [ t1, fill "#5fb4cb", d "m156 0h124v124c-41-41-83-83-124-124" ] []
                , path [ t1, fill "#596378", d "m0 7.7c44 44 88 88 133 133-44 44-88 88-133 133v-265" ] []
                , path [ t1, fill "#f0ad00", d "m80 72c40-.1 81-.1 121 0-20.3 20-40 40-61 61-20-20-40-40-60-61" ] []
                , path [ t1, fill "#7ed13b", d "m214 74c22 21.8 43.8 43.7 66 66v1.1c-22 21.9-43.8 43.9-65.9 65.8-22-22-44-44-66-66 22-22 44-44 66-66" ] []
                , path [ t1, fill "#5fb4cb", d "m8.6 280c43.9-43.9 88-88 132-132 44 43.8 88 88 132 132h-264" ] []
                , path [ t1, fill "#f0ad00", d "m222 214c19-19.7 38.8-38.8 58-58v116c-19.4-19.3-38.6-38.8-58-58" ] []
                ]
            ]


app =
    StartApp.start
        { init = init
        , update = update
        , view = view
        , inputs = []
        }


main =
    app.html


port tasks : Signal (Task.Task Effects.Never ())
port tasks =
    app.tasks
