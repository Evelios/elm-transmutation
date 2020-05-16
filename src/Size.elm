module Size exposing
    ( Size
    , height
    , max
    , min
    , scale
    , setHeight
    , setWidth
    , size
    , width
    )

import Quantity exposing (Quantity)


type alias Size units =
    { width : Quantity Float units
    , height : Quantity Float units
    }


size : Quantity Float units -> Quantity Float units -> Size units
size w h =
    Size w h


width : Size units -> Quantity Float units
width s =
    s.width


height : Size units -> Quantity Float units
height s =
    s.height


setHeight : Quantity Float units -> Size units -> Size units
setHeight h old =
    { old | height = h }


setWidth : Quantity Float units -> Size units -> Size units
setWidth w old =
    { old | width = w }


scale : Float -> Size units -> Size units
scale ammount from =
    size
        (Quantity.multiplyBy ammount from.width)
        (Quantity.multiplyBy ammount from.height)


min : Size units -> Quantity Float units
min s =
    Quantity.min s.width s.height


max : Size units -> Quantity Float units
max s =
    Quantity.max s.width s.height
