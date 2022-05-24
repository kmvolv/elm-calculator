module Deps exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes
import String exposing (..)
import Tuple exposing (first,second)
import Round 

import List exposing (..)
import List.Extra exposing (..)

import Basics.Extra 
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
type Msg = Add | Subtract | Multiply | Divide | Modulo | Equals | Number Int | Point | AC | Zero | Decrement | Init Int

-- Function Container 
type alias Calculator =
    { add : Float -> Float -> Float
    , subtract : Float -> Float -> Float
    , multiply : Float -> Float -> Float
    , divide : Float -> Float -> Float
    , modulo : Float -> Float -> Float
    }

getMulti : Float -> Float -> Float 
getMulti x y = 
    getFloat(Round.round 5 (x * y))

getDivide : Float -> Float -> Float
getDivide x y = 
    getFloat(Round.round 5 (x / y))

-- Functions required
calculator : Calculator
calculator =
    { add = (\x y -> x + y)
    , subtract = (\x y -> x - y)
    , multiply = (\x y -> getMulti x y)
    , divide = (\x y -> getDivide x y)
    , modulo = (\x y -> Basics.Extra.fractionalModBy y x)
    }


lstFuncs: List((Msg,String))
lstFuncs = [
                (AC,"AC")
                , (Number 7,"7")
                , (Number 4,"4")
                , (Number 1,"1")
                , (Decrement,"")

                , (Divide,"÷")
                , (Number 8,"8")
                , (Number 5,"5")
                , (Number 2,"2")
                , (Number 0,"0")

                , (Multiply,"x")
                , (Number 9,"9")
                , (Number 6,"6")
                , (Number 3,"3")
                , (Point,".")

                , (Modulo,"%")
                , (Subtract,"-")
                , (Add,"+")
                , (Equals, "=")
            ]

getAtpos: Int -> (Msg,String)
getAtpos pos = 
    Maybe.withDefault (Zero,"") (getAt pos lstFuncs)

genBtnsCol: Int -> Int -> Int -> Int -> List(Svg Msg)
genBtnsCol idx rows cols pos= 
    let
        yttrans = fromInt (pos+25)
        ytrans = fromInt pos
        func = first (getAtpos idx)
        content = second (getAtpos idx)
    in  
    if cols <= 0 then []
    else 
        if (rows == 1 && cols == 1) then 
            [
                Svg.path [class "btn-wide", onClick func, d (" M 0 "++ (ytrans) ++" h 50 a 2 2 0 0 1 2 2 v 116 a 2 2 0 0 1 -2 2 h -50 a 2 2 0 0 1 -2 -2 v -116 a 2 2 0 0 1 2 -2")]
                []
                , text_ [
                    x "25"
                    , y (fromInt (pos+56))
                    , fill "black"
                    , textAnchor "middle"
                    , dominantBaseline "central"
                    , Svg.Attributes.cursor "pointer"
                    , class "btn-text"
                ]
                [Svg.text content]
            ]
        else if (rows == 4 && cols == 1) then
            [
                Svg.path [class "btn-cols", onClick func, d (" M 0 "++ (ytrans) ++" h 50 a 2 2 0 0 1 2 2 v 46 a 2 2 0 0 1 -2 2 h -50 a 2 2 0 0 1 -2 -2 v -46 a 2 2 0 0 1 2 -2")]
                []
                , svg [viewBox "0 0 400 400", transform "translate(7,240)"]
                    [
                        Svg.path [d "M10.625,5.09L0,22.09l10.625,17H44.18v-34H10.625z M42.18,37.09H11.734l-9.375-15l9.375-15H42.18V37.09z"]
                        []
                        , Svg.polygon [points "18.887,30.797 26.18,23.504 33.473,30.797 34.887,29.383 27.594,22.09 34.887,14.797 33.473,13.383 26.18,20.676 18.887,13.383 17.473,14.797 24.766,22.09 17.473,29.383"]
                        []
                    ]
            ]
        else [
            Svg.path [class "btn-cols", onClick func, d (" M 0 "++ (ytrans) ++" h 50 a 2 2 0 0 1 2 2 v 46 a 2 2 0 0 1 -2 2 h -50 a 2 2 0 0 1 -2 -2 v -46 a 2 2 0 0 1 2 -2")]
            []
            , text_ [
                x "25"
                , y yttrans
                , fill "black"
                , textAnchor "middle"
                , dominantBaseline "central"
                , Svg.Attributes.cursor "pointer"
                , class "btn-text"
            ]
            [Svg.text content]
        ] 
    ++ (genBtnsCol (idx+1) (rows) (cols - 1) (pos+70))
         

genBtns: Int -> Int ->Int -> Int -> List(Svg Msg)
genBtns idx rows cols pos = 
    let
        xtrans = fromInt(pos)
    in
    if cols <= 0 then []
    else [
        if cols == 1 then Svg.g[transform ("translate("++ xtrans ++",0)")](genBtnsCol idx cols (rows - 1) 0)
         else Svg.g[transform ("translate("++ xtrans ++",0)")](genBtnsCol idx cols rows 0)
        ] ++ genBtns (idx + rows) (rows) (cols - 1) (pos + 70)


btnContainer: Int -> Int -> Int -> Int -> Html Msg
btnContainer w h rows cols = 
    let
        wdth = fromInt(w)
        hght = fromInt(h)   
    in
    
    div[Html.Attributes.style "display" "flex", Html.Attributes.style "justify-content" "center", Html.Attributes.style "flex-grow" "1"]
        [
            svg [ viewBox "-5 -5 300 400", width wdth, height hght, fill "none", stroke "black", class "btn-container", transform "translate(0,20)"]
                 [
                    Svg.g[transform ("translate(5,0)")]
                        
                            (genBtns 0 rows cols 0)
                 ]
        ]

displayAdd: String -> String -> Html Msg
displayAdd str memstr = 
    div[class "Display"]
        [
            svg [ viewBox "0 0 100 100", width "300", height "200", fill "none", stroke "black", class "disp", transform "translate(1,0)" ]
            [ 
                Svg.g[] 
                    [
                    Svg.path [ d "M-20 20h 140a 2 2 0 0 1 2 2v 50a 2 2 0 0 1 -2 2h -140a 2 2 0 0 1 -2 -2v -50a 2 2 0 0 1 2 -2"]
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
                        , y "60"
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

handleDec: Model -> (Model, Cmd Msg)
handleDec model = 
    ({
        model | display = if (model.display == "" || model.pressedEq == True) then
                            model.display
                            else dropRight 1 model.display
    }, Cmd.none)