module Main exposing (..)

import Browser
import Deps exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Basics.Extra 

import Html.Events exposing (onClick)
import Html.Attributes as HA
import String exposing (..)
import Tuple exposing (first,second)

import List exposing (..)
import List.Extra exposing (..)


import Svg exposing (..)
import Svg.Attributes as SA exposing (..)

import List exposing (..)
import List.Extra exposing (..)

-- Model
type alias Model = 
    {
        display: String
        , history: String
        , op: Float -> Float -> Float
        , mem: Float
        , perform: Bool
        , pressedEq: Bool
        , pressedOp: Bool
    }

-- Message Var
type Msg 
    = Add 
    | Subtract 
    | Multiply 
    | Divide 
    | Modulo 
    | Equals 
    | Number Int 
    | Point 
    | AC 
    | Zero 
    | Decrement 
    | Init Int

-- Function Container 
type alias Calculator =
    { add : Float -> Float -> Float
    , subtract : Float -> Float -> Float
    , multiply : Float -> Float -> Float
    , divide : Float -> Float -> Float
    , modulo : Float -> Float -> Float
    }

-- Functions required
calculator : Calculator
calculator =
    { add = (\x y -> getAdd x y)
    , subtract = (\x y -> getSub x y)
    , multiply = (\x y -> getMulti x y)
    , divide = (\x y -> getDivide x y)
    , modulo = (\x y -> Basics.Extra.fractionalModBy y x)
    }

-- Init
init : () -> ( Model, Cmd Msg )
init _ =
    ( { display = ""
      , history = ""
      , op = \x y -> x*0 +y
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
    Html.node tag attrs children

-- List of button funcs to be added
lstFuncs: List((Msg,String))
lstFuncs = [
                (AC,"AC")
                , (Number 7,"7")
                , (Number 4,"4")
                , (Number 1,"1")
                , (Modulo,"%")

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

                , (Decrement,"")
                , (Subtract,"-")
                , (Add,"+")
                , (Equals, "=")
            ]

-- Get item at given position of list
getAtpos: Int -> (Msg,String)
getAtpos pos = 
    Maybe.withDefault (Zero,"") (getAt pos lstFuncs)


-- Add Display and Text content
displayAdd: String -> String -> Html Msg
displayAdd str memstr = 
    div[HA.class "Display"]
        [
            svg [ viewBox "0 0 100 100", SA.width "300", SA.height "200", fill "none", stroke "black", SA.class "disp", transform "translate(1,0)" ]
            [ 
                Svg.g[] 
                    [
                    rect [x "-20", y "20", rx "2", ry "2", SA.width "142", SA.height "52"]
                    []
                    , text_ [
                        x "-10"
                        , y "30"
                        , fill "black"
                        , dominantBaseline "central"
                        , fontSize "10px"
                        , textAnchor "start"
                        , fontWeight "lighter"
                        , SA.class "disp-upper"
                    ]
                    [Html.text (memstr)]
                    , text_ [
                        x "118"
                        , y "58"
                        , fill "black"
                        , dominantBaseline "central"
                        , fontSize "20px"
                        , textAnchor "end"
                        , fontWeight "lighter"
                        , SA.class "disp-lower"
                    ]
                    [Html.text str]
                ]
            ]
        ]

-- Generates a column of buttons
genBtnsCol: Int -> Int -> Int -> Int -> List(Svg Msg)
genBtnsCol idx rows cols pos= 
    let
        yttrans = fromInt (pos+25)
        ytrans = fromInt pos
        func = first (getAtpos idx)
        content = second (getAtpos idx)
    in  
    if rows <= 0 then []
    else 
        (genBtnsCol (idx+1) (rows - 1) (cols) (pos+70))
        ++
        -- For Equals
        if (rows == 1 && cols == 1) then 
            [
                rect [SA.class "btn-wide", onClick func, x "0", y ytrans, SA.width "50", SA.height "120", rx "2", ry "2"]
                []
                , text_ [
                    x "25"
                    , y (fromInt (pos+56))
                    , fill "black"
                    , textAnchor "middle"
                    , dominantBaseline "central"
                    , SA.cursor "pointer"
                    , SA.class "btn-text"
                ]
                [Svg.text content]
            ]
        -- For Decrement
        else if (cols == 1 && rows == 4) then
            [
                rect [SA.class "btn-cols", onClick func, x "0", y ytrans, SA.width "50", SA.height "50", rx "2", ry "2"]
                []
                , svg [viewBox "0 0 80 10", transform "translate(10,-10)", SA.cursor "pointer", SA.width "51", SA.height "51"]
                    [
                        -- Svg for decrement icon
                        Svg.path [d "M10.625,5.09L0,22.09l10.625,17H44.18v-34H10.625z M42.18,37.09H11.734l-9.375-15l9.375-15H42.18V37.09z"]
                        []
                        , Svg.polygon [points "18.887,30.797 26.18,23.504 33.473,30.797 34.887,29.383 27.594,22.09 34.887,14.797 33.473,13.383 26.18,20.676 18.887,13.383 17.473,14.797 24.766,22.09 17.473,29.383"]
                        []
                    ]
            ]
        else 
            -- For the other buttons
        [
            rect [SA.class "btn-cols", onClick func, x "0", y ytrans, SA.width "50", SA.height "50", rx "2", ry "2"]
            []
            , text_ [
                x "25"
                , y yttrans
                , fill "black"
                , textAnchor "middle"
                , dominantBaseline "central"
                , SA.cursor "pointer"
                , SA.class "btn-text"
            ]
            [Svg.text content]
        ] 
         

-- Generates the button grid
genBtns: Int -> Int -> Int -> Int -> List(Svg Msg)
genBtns idx rows cols pos = 
    let
        xtrans = fromInt(pos)
    in
    if cols <= 0 then []
    else 
        genBtns (idx + rows) (rows) (cols - 1) (pos + 70) ++
        [
            if cols == 1 then Svg.g[transform ("translate("++ xtrans ++",0)")](genBtnsCol idx (rows - 1) cols 0) 
            else Svg.g[transform ("translate("++ xtrans ++",0)")](genBtnsCol idx rows cols 0)
        ] 

-- Add Buttons of calc
calciInter: String -> String -> Int -> Int -> Int -> Int -> Html Msg
calciInter memstr str w h rows cols = 
    let
        wdth = fromInt(w)
        hght = fromInt(h)   
    in
    
    div[Html.Attributes.style "display" "flex", Html.Attributes.style "justify-content" "center", Html.Attributes.style "flex-grow" "1"]
        [
            svg [ viewBox "-5 -5 300 400", SA.width wdth, SA.height hght, fill "none", stroke "black", SA.class "btn-container", transform "translate(0,0)"]
                    [
                        
                        Svg.g[][
                                rect [x "0", y "0", SA.width "280", SA.height "100", rx "2", ry "2", transform "translate(-4,-30)"][]
                                , text_ [
                                    x "5"
                                    , y "-5"
                                    , fill "black"
                                    , dominantBaseline "central"
                                    , fontSize "10px"
                                    , textAnchor "start"
                                    , fontWeight "lighter"
                                    , SA.class "disp-upper"
                                ]
                                [Html.text (memstr)]
                                , text_ [
                                    x "265"
                                    , y "40"
                                    , fill "black"
                                    , dominantBaseline "central"
                                    , fontSize "40px"
                                    , textAnchor "end"
                                    , fontWeight "bold"
                                    , SA.class "disp-lower"
                                ]
                                [Html.text str]
                            
                            , Svg.g[transform ("translate(5,90)")]
                            (genBtns 0 rows cols 0) 
                        ]
                    ]
        ]

-- View
view : Model -> Html Msg
view model =
    div []
        [ stylesheet
        , div [ SA.class "Calci" ]
            [ 
                calciInter model.history model.display 300 500 5 4
            ]
        ]


-- Clicks on an operation button
mathOp: Model -> (Float -> Float -> Float) -> String -> (Model, Cmd Msg)
mathOp model func symb = 
    ({
        model | op = func
        , pressedEq = False
        , history = if (model.pressedEq == False && model.pressedOp == False) then model.history ++ model.display ++ " "  ++ (symb) ++ " "
                    else if model.pressedOp == False then model.display ++ " "  ++ (symb) ++ " "
                        else dropRight 2 model.history ++ (symb) ++ " "
        , mem = if (model.perform == False) then getFloat model.display 
                else if model.pressedOp == False then getFloat (calc model)
                else model.mem
        , perform = True
        , display = ""
        , pressedOp = True
    }, Cmd.none)

-- Clicks on a number button
addNumber: Model -> Int -> (Model, Cmd Msg)
addNumber model i = 
    ({
        model | display = model.display ++ fromInt(i)
        , pressedOp = False
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
        , pressedOp = False
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
        , history = if model.pressedEq == False then model.history ++ model.display
                    else model.display
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
        , op = (\x y -> x*0 + y)
        , history = ""
        , pressedEq = False
        , pressedOp = False
    }, Cmd.none)

-- Click on Zero
addZero: Model -> (Model, Cmd Msg)
addZero model = 
    ({
        model | display = if model.display == "" || model.display == "0" then
                            "0"
                            else model.display ++ "0"
        , pressedOp = False
    }, Cmd.none)

-- Click on Decrement
handleDec: Model -> (Model, Cmd Msg)
handleDec model = 
    ({
        model | display = if (model.display == "" || model.pressedEq == True) then
                            model.display
                            else dropRight 1 model.display
        , pressedOp = False
    }, Cmd.none)


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
main: Program() Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , subscriptions = \_ -> Sub.none
        , update = update
        }
