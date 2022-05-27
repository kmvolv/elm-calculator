module Main exposing (..)

import Browser
import Deps exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)

-- Init
init : () -> ( Model, Cmd Msg )
init _ =
    ( { display = ""
      , history = ""
      , op = \x y -> y
      , mem = 0
      , perform = False
      , pressedEq = False
      , pressedOp = False
      }
    , Cmd.none
    )


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
view : Model -> Html Msg
view model =
    div []
        [ stylesheet
        , div [ class "Calci" ]
            [ displayAdd model.display model.history
            , btnContainer 300 400 5 4
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
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

        Modulo ->
            mathOp model calculator.modulo "%"

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

        Decrement ->
            handleDec model

        Init _ ->
            ( model, Cmd.none )

-- Main
main =
    Browser.element
        { init = init
        , view = view
        , subscriptions = \_ -> Sub.none
        , update = update
        }
