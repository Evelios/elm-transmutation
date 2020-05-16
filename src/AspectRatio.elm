module AspectRatio exposing
    ( AspectRatio
    , aspectRatio
    , fromSize
    , inAspectRatio
    , toSizeFromBase
    , x
    , y
    )

import Quantity exposing (Quantity)
import Size exposing (Size)


type alias AspectRatio =
    { x : Float
    , y : Float
    }


aspectRatio : Float -> Float -> AspectRatio
aspectRatio xin yin =
    let
        denom =
            min xin yin
    in
    AspectRatio (xin / denom) (yin / denom)


x : AspectRatio -> Float
x ratio =
    .x ratio


y : AspectRatio -> Float
y ratio =
    .y ratio


fromSize : Size units -> AspectRatio
fromSize { width, height } =
    let
        largest =
            Quantity.max width height

        xAspect =
            Quantity.ratio largest height

        yAspect =
            Quantity.ratio largest width
    in
    AspectRatio xAspect yAspect


toSizeFromBase : Quantity Float units -> AspectRatio -> Size units
toSizeFromBase min aspect =
    Size.size (Quantity.multiplyBy aspect.x min) (Quantity.multiplyBy aspect.y min)


inAspectRatio : AspectRatio -> Size units -> Size units
inAspectRatio ratio { width, height } =
    let
        widthNew =
            if x ratio < y ratio then
                Quantity.divideBy (y ratio) height

            else
                Quantity.divideBy (y ratio) width

        heightNew =
            if y ratio > x ratio then
                Quantity.divideBy (x ratio) height

            else
                Quantity.divideBy (x ratio) width
    in
    Size.size widthNew heightNew
