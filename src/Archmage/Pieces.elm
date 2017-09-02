----------------------------------------------------------------------
--
-- Pieces.elm
-- Archmage piece drawing
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

module Archmage.Pieces exposing ( drawPiece )

import Archmage.Types exposing ( Piece(..), Color(..) )

import Svg exposing ( Svg, line, g, path )
import Svg.Attributes exposing ( x, y, width, height
                               , cx, cy, r
                               , x1, y1, x2, y2
                               , d
                               , fill, stroke, strokeWidth
                               , fillOpacity, opacity
                               )

colorString : Color -> String
colorString color =
    case color of
        Black -> "black"
        White -> "white"

opacity : String
opacity =
    "0.4"

oppositeColor : Color -> Color
oppositeColor color =
    case color of
        White -> Black
        Black -> White

bodyOpacity : Color -> String
bodyOpacity color =
    case color of
        Black -> opacity
        White -> "1"

fillAndOpacity : Color -> (String, String)
fillAndOpacity color =
    let otherColor = oppositeColor color
    in
        ( colorString otherColor
        , bodyOpacity otherColor
        )

drawCircle : Color -> Int -> Int -> Int -> Svg msg
drawCircle color centerx centery radius =
    let (f, o) = fillAndOpacity color
        sr = round ((toFloat radius) * 0.8)
    in
        Svg.circle [ cx (toString centerx)
                   , cy (toString centery)
                   , r (toString sr)
                   , fill f
                   , fillOpacity o
                   ]
            []

type alias PathElement number =
    { letter : String
    , numbers : List number
    }

type alias PathSpec number =
    List (PathElement number)

scaleInt : Float -> Int -> String
scaleInt f n =
    toString <| f * (toFloat n)

scalePathElement : Float -> PathElement Int -> PathElement String
scalePathElement scale element =
    { letter = element.letter
    , numbers = List.map (scaleInt scale) element.numbers
    }

scalePathSpec : Float -> PathSpec Int -> PathSpec String
scalePathSpec scale spec =
    List.map (scalePathElement scale) spec    

pathElementToString : PathElement String -> String
pathElementToString element =
    element.letter ++ String.join " " element.numbers

pathSpecToString : PathSpec String -> String
pathSpecToString spec =
    String.join " " <| List.map pathElementToString spec

drawCup : Color -> Int -> Int -> Int -> Svg msg
drawCup color centerx centery radius =
    let (f, o) = fillAndOpacity color
        cr = (radius * 2) // 3
        scr = toString cr
        tlx = toString (centerx - cr)
        tly = toString (centery - cr)
        trx = toString (centerx + cr)
        try = tly
        bmx = centerx
        by = centery + cr
        br = radius // 2
        blx = bmx - br
        brx = bmx + br
        paths = "M " ++ tlx ++ " " ++ tly ++
                " A " ++ scr ++ " " ++ scr ++
                " 0 1 0 " ++
                trx ++ " " ++ try ++
                " Z"
    in
        g []
            [ path [ d paths
                   , fill f
                   , fillOpacity o
                   ]
                  []
            , line [ x1 <| toString centerx
                   , y1 <| toString centery
                   , x2 <| toString centerx
                   , y2 <| toString by
                   , strokeWidth "3"
                   ]
                []
            , line [ x1 <| toString blx
                   , y1 <| toString by
                   , x2 <| toString brx
                   , y2 <| toString by
                   , strokeWidth "3"
                   ]
                []
            ]

{-
<!-- Generated with http://jxnblk.com/paths -->
<svg
  xmlns='http://www.w3.org/2000/svg'
  viewBox='0 0 64 64'
  width='64' height='64'
  fill='currentcolor'>
  <path d='M 0,54 l 0,-20 q 5,-22 8,-2 q 3,-45 6,0 q 4,-60 8,0 l 0,-2 q 3,-46 6,2 q 2,-26 4,-2 l 0,22 l -4,6 l -10,0 Z' />
</svg>
-}
drawHand: Color -> Int -> Int -> Int -> Svg msg
drawHand color centerx centery radius =
    drawCircle color centerx centery radius

pieceBody : Piece -> Color -> Int -> Int -> Int -> Svg msg
pieceBody piece color centerx centery radius =
    case piece of
        CupPiece ->
            drawCup color centerx centery radius
        _ ->
            drawCircle color centerx centery radius

drawPiece : Piece -> Color -> Int -> Int -> Int -> Svg msg
drawPiece piece color ix iy size =
    let radius = size // 2
        centerx = ix + radius
        centery = iy + radius
        ssize = toString size
    in
        g []
            [ Svg.rect [ x <| toString ix
                       , y <| toString iy
                       , width ssize
                       , height ssize
                       , fill <| colorString color
                       , fillOpacity opacity
                       , strokeWidth "0"
                       ]
                  []
            , pieceBody piece color centerx centery radius
            ]
    
