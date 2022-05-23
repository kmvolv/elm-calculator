module Deps exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import String exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)

-- Model
type alias Model = 
    {
        display: String
        , history: String
        , op: Float -> Float -> Float
        , mem: Float
        , perform: Bool
        , pressedEq: Bool
    }

-- Message Var
type Msg = Add | Subtract | Multiply | Divide | Equals | Number Int | Point | AC | Zero

-- Function Container 
type alias Calculator =
    { add : Float -> Float -> Float
    , subtract : Float -> Float -> Float
    , multiply : Float -> Float -> Float
    , divide : Float -> Float -> Float
    }

-- Functions required
calculator : Calculator
calculator =
    { add = (\x y -> x + y)
    , subtract = (\x y -> x - y)
    , multiply = (\x y -> x * y)
    , divide = (\x y -> x / y)
    }

-- To add buttons of calculator
buttonAdd: Msg -> String -> Html Msg
buttonAdd func char = 
    div[]
        [
           svg [ viewBox "0 0 24 24", width "64", height "64", onClick func, fill "none", stroke "black", class "btn"]
            [ Svg.path [ d " M 4 4 h 16 a 2 2 0 0 1 2 2 v 14 a 2 2 0 0 1 -2 2 h -16 a 2 2 0 0 1 -2 -2 v -14 a 2 2 0 0 1 2 -2" ]
                []
                , text_ [
                    x "12"
                    , y "12"
                    , fill "black"
                    , textAnchor "middle"
                    , dominantBaseline "central"
                    , Svg.Attributes.cursor "pointer"
                ]
                [Html.text char]
            ]

        ]

buttonAddWide: Msg -> String -> Html Msg
buttonAddWide func char = 
    div[]
        [
           svg [ viewBox "0 0 24 48", width "64", height "128", onClick func, fill "none", stroke "black", class "btn btn-wide"]
            [ Svg.path [ d " M 4 4 h 16 a 2 2 0 0 1 2 2 v 39 a 2 2 0 0 1 -2 2 h -16 a 2 2 0 0 1 -2 -2 v -39 a 2 2 0 0 1 2 -2" ]
                []
                , text_ [
                    x "12"
                    , y "24"
                    , fill "black"
                    , textAnchor "middle"
                    , dominantBaseline "central"
                ]
                [Html.text char]
            ]

        ]

displayAdd: String -> String -> Html Msg
displayAdd str memstr = 
    div[class "Display"]
        [
            svg [ viewBox "0 0 100 100", width "300", height "200", fill "none", stroke "black", class "disp" ]
            [ 
                Svg.g[] 
                    [
                    Svg.path [ d "M-20 20h 140a 2 2 0 0 1 2 2v 40a 2 2 0 0 1 -2 2h -140a 2 2 0 0 1 -2 -2v -40a 2 2 0 0 1 2 -2"]
                    []
                    , text_ [
                        x "-15"
                        , y "30"
                        , fill "black"
                        , textAnchor "middle"
                        , dominantBaseline "central"
                        , fontSize "10px"
                        , textAnchor "start"
                        , fontWeight "lighter"
                        , class "disp-upper"
                    ]
                    [Html.text (memstr)]
                    , text_ [
                        x "120"
                        , y "50"
                        , fill "black"
                        , textAnchor "middle"
                        , dominantBaseline "central"
                        , fontSize "20px"
                        , textAnchor "end"
                        , fontWeight "lighter"
                        , class "disp-lower"
                    ]
                    [Html.text str]
                ]
            ]
        ]

-- Convert string to Float
getFloat: String -> Float
getFloat str = 
    Maybe.withDefault 0 (String.toFloat str)

-- Clicks on an operation button
mathOp: Model -> (Float -> Float -> Float) -> String -> (Model, Cmd Msg)
mathOp model func symb= 
    ({
        model | op = func
        , pressedEq = False
        , history = if (model.pressedEq == False) then model.history ++ model.display ++ " "  ++ (symb) ++ " "
                    else model.display ++ " "  ++ (symb) ++ " "
        , mem = if (model.perform == False) then getFloat model.display 
                else getFloat (calc model)
        , perform = True
        , display = ""
    }, Cmd.none)

-- Clicks on a number button
addNumber: Model -> Int -> (Model, Cmd Msg)
addNumber model i = 
    ({
        model | display = model.display ++ fromInt(i)
    }, Cmd.none)

-- Function to append string with '.'
appendStr: String -> String
appendStr str = 
    if String.contains "." str then
        str
    else str ++ "."

-- Click on '.'
addPoint: Model -> (Model, Cmd Msg)
addPoint model = 
    ({
        model | display = if model.display == "" then "0."
                            else  appendStr model.display
    }, Cmd.none)

-- To carry out operation
calc: Model -> String
calc model = 
    if model.perform == True then 
        fromFloat <| model.op model.mem (getFloat model.display)
    else fromFloat <| model.op (getFloat model.display) model.mem

-- Click on Equals
handleEqual: Model -> (Model, Cmd Msg)
handleEqual model = 
    ({
        model | display = calc model
        , history = model.history ++ model.display
        , perform = False
        , mem = if model.perform == True then getFloat(model.display)
                else model.mem
        , pressedEq = True
    }, Cmd.none)

-- Click on AC
allClear: Model -> (Model, Cmd Msg)
allClear model = 
    ({
        model | display = ""
        , perform = False
        , mem = 0
        , op = (\x y -> y)
        , history = ""
    }, Cmd.none)

-- Click on Zero
addZero: Model -> (Model, Cmd Msg)
addZero model = 
    ({
        model | display = if model.display == "" || model.display == "0" then
                            "0"
                            else model.display ++ "0"
    }, Cmd.none)