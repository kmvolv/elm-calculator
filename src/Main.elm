module Main exposing (..)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Deps exposing (..)

-- Main
main = 
    Browser.element
        {
            init = init
            , view = view
            , subscriptions = subscriptions
            , update = update
        }

-- Init
init: () -> (Model, Cmd Msg)
init _ = 
    ({
        display = ""
        , history = ""
        , op = (\x y -> y)
        , mem = 0
        , perform = False
        , pressedEq = False
    }, Cmd.none)



stylesheet : Html Msg
stylesheet =
    let
        tag =
            "link"

        attrs =
            [ attribute "Rel" "stylesheet"
            , attribute "property" "stylesheet"
            , attribute "href" "styles.css"
            ]

        children =
            []
    in
        node tag attrs children


-- View
view: Model -> Html Msg
view model = 
    div []
            [
                stylesheet
                , div[class "Calci"]
                    [
                        displayAdd model.display model.history
                        , div[style "display" "inline-flex", style "justify-content" "space-evenly"]
                        [
                            div[style "display" "flex", style "flex-direction" "column"]
                                [
                                    buttonAdd AC "AC"
                                    , buttonAdd (Number 7) "7"
                                    , buttonAdd (Number 4) "4"
                                    , buttonAdd (Number 1) "1"
                                    , buttonAdd Zero "~"
                                ]

                            , div[style "display" "flex", style "flex-direction" "column"]
                                [
                                    buttonAdd Divide "÷"
                                    , buttonAdd (Number 8) "8"
                                    , buttonAdd (Number 5) "5"
                                    , buttonAdd (Number 2) "2"
                                    , buttonAdd (Number 0) "0"
                                ]

                            , div[style "display" "flex", style "flex-direction" "column"]
                                [ 
                                    buttonAdd Multiply "x"
                                    , buttonAdd (Number 9) "9"
                                    , buttonAdd (Number 6) "6"
                                    , buttonAdd (Number 3) "3"
                                    , buttonAdd Point "."
                                ]

                            , div[style "display" "flex", style "flex-direction" "column"]
                                [ 
                                    buttonAdd Zero "~"
                                    , buttonAdd Subtract "-"
                                    , buttonAdd Add "+"
                                    , buttonAddWide Equals "="
                                ]
                        ]
                    ] 
            ]

update: Msg -> Model -> (Model, Cmd Msg)
update message model =
    case message of 
        Add -> 
            mathOp model calculator.add "+"
        Subtract -> 
            mathOp model calculator.subtract "-"
        Multiply ->
            mathOp model calculator.multiply "x"
        Divide ->
            mathOp model calculator.divide "÷"
        Number i ->
            addNumber model i
        Point ->
            addPoint model
        Equals ->
            handleEqual model
        AC ->
            allClear model
        Zero ->
            addZero model

subscriptions: Model -> Sub Msg
subscriptions model =
    Sub.none