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

import Char

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

type alias IntPair =
    (Int, Int)

type alias FloatPair =
    (Float, Float)

type alias StringPair =
    (String, String)

type alias PathElement number =
    (Char, List (number, number))

type alias PathSpec number =
    List (PathElement number)

scaleInt : Float -> Float -> Int -> String
scaleInt offset scale n =
    toString <| scale * ((toFloat n) + offset)

scaleIntPair : FloatPair -> Float -> IntPair -> StringPair
scaleIntPair offset scale n =
    let (ox, oy) = offset
        (nx, ny) = n
    in
        (scaleInt ox scale nx, scaleInt oy scale ny)

scalePathElement : FloatPair -> Float -> PathElement Int -> PathElement String
scalePathElement offset scale element =
    let (letter, pairs) = element
        off = if Char.isLower letter then
                  (0, 0)        --don't offset relative path elements
              else
                  offset
    in
        (letter, List.map (scaleIntPair off scale) pairs)

scalePathSpec : FloatPair -> Float -> PathSpec Int -> PathSpec String
scalePathSpec offset scale spec =
    List.map (scalePathElement offset scale) spec    

type alias PathSize =
    (Int, Int)

normalizePathSpec : Int -> PathSize -> PathSpec Int -> PathSpec String
normalizePathSpec size pathSize spec =
    let (sx, sy) = pathSize
        fsx = toFloat sx
        fsy = toFloat sy
        fps = toFloat (max sx sy)
        offset = if sx < sy then
                     ((fsy - fsx) / 2.0, 0.0)
                 else
                     (0.0, (fsx - fsy) / 2.0)
        scale = (toFloat size) / fps
    in
        scalePathSpec offset scale spec

pairToString : StringPair -> String
pairToString pair =
    let (px, py) = pair
    in
        px ++ "," ++ py

pathElementToString : PathElement String -> String
pathElementToString element =
    let (letter, pairs) = element
    in
        (String.fromChar letter) ++
            String.join " " (List.map pairToString pairs)

pathSpecToString : PathSpec String -> String
pathSpecToString spec =
    String.join " " <| List.map pathElementToString spec

{-
<!-- Generated with http://jxnblk.com/paths -->
<svg
  xmlns='http://www.w3.org/2000/svg'
  viewBox='0 0 32 58'
  width='32' height='58'
  fill='currentcolor'>
  <path d='M 0,54 l 0,-20 q 5,-22 8,-2 q 3,-45 6,0 q 4,-60 8,0 l 0,-2 q 3,-46 6,2 q 2,-26 4,-2 l 0,22 l -4,6 l -10,0 Z' />
</svg>
-}
handPath : PathSpec Int
handPath =
    [ ('M', [(0,54)])
    , ('l', [(0,-20)])
    , ('q', [(5,-22), (8,-2)])
    , ('q', [(3,-45), (6,0)])
    , ('q', [(4,-60), (8,0)])
    , ('l', [(0,-2)])
    , ('q', [(3,-46), (6,2)])
    , ('q', [(2,-26), (4,-2)])
    , ('l', [(0,22)])
    , ('l', [(-4,6)])
    , ('l', [(-10,0)])
    , ('Z', [])
    ]

handSize : PathSize
handSize =
    (32, 58)

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
    
