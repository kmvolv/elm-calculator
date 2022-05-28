module Deps exposing (..)

import Html exposing (..)
import Round


-- Precision for addition function
getAdd : Float -> Float -> Float
getAdd x y = 
    getFloat(Round.round 5 (x + y))

-- Precision for subtraction function
getSub : Float -> Float -> Float
getSub x y = 
    getFloat(Round.round 5 (x - y))

-- Precision for multiply function
getMulti : Float -> Float -> Float 
getMulti x y = 
    getFloat(Round.round 5 (x * y))

-- Precision for divide function
getDivide : Float -> Float -> Float
getDivide x y = 
    getFloat(Round.round 5 (x / y))

-- Convert string to Float
getFloat: String -> Float
getFloat str = 
    Maybe.withDefault 0 (String.toFloat str)

