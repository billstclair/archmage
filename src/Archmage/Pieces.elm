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

import Svg exposing ( Svg, Attribute, line, g, path )
import Svg.Attributes exposing ( x, y, width, height
                               , cx, cy, r
                               , x1, y1, x2, y2
                               , d
                               , fill, stroke, strokeWidth
                               , fillOpacity, opacity
                               , transform
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

scaleRadius : Int -> Int
scaleRadius radius =
    (radius * 4) // 5

drawCup : Color -> Int -> Int -> Int -> Svg msg
drawCup color centerx centery radius =
    let (f, o) = fillAndOpacity color
        cr = scaleRadius radius
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

type alias PathSize =
    (Int, Int)

normalizationTransform : Int -> PathSize -> Attribute msg
normalizationTransform  size pathSize =
    let (sx, sy) = pathSize
        fsx = toFloat sx
        fsy = toFloat sy
        fps = toFloat (max sx sy)
        fsize = toFloat size
        sc = fsize / fps
        (ox, oy) = if sx < sy then
                       (toString <| (fsize - (sc * fsx)) / 2.0, "0")
                   else
                       ("0", toString <| (fsize - (sc * fsy)) / 2.0)
        scale = toString sc
    in
        transform ("translate(" ++ ox ++ " " ++ oy ++ ") " ++
                   "scale(" ++ scale ++ ")"
                  )

drawScaledPath : Int -> PathSize -> String -> Svg msg
drawScaledPath size pathSize theD =
    g [ normalizationTransform size pathSize ]
        [ path [ d theD ] [] ]

drawPathD: Color -> Int -> Int -> Int -> PathSize -> String -> Svg msg
drawPathD color centerx centery radius pathSize pathD =
    let sr = scaleRadius radius
        ox = toString (centerx - sr)
        oy = toString (centery - sr)
        size = 2 * sr
        (f, o) = fillAndOpacity color
    in
        g [ transform <| "translate(" ++ ox ++ " " ++ oy ++ ")"
          , fill f
          , fillOpacity o
          ]
            [ drawScaledPath size pathSize pathD ]

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
handSize : PathSize
handSize =
    (32, 58)

handD : String
handD =
    "M 5,54 l -5,-20 " ++
    "q 5,-22 8,-2 " ++          --thumb
    "q 3,-45 6,0 " ++           --index
    "q 4,-60 8,0 " ++           --middle
    "l 0,-2 q 3,-46 6,2 " ++    --ring
    "q 2,-26 4,-2 " ++          --little
    "l 0,22 l -4,6 l -10,0 Z"

drawHand : Color -> Int -> Int -> Int -> Svg msg
drawHand color centerx centery radius =
    drawPathD color centerx centery radius handSize handD

swordSize : PathSize
swordSize =
    (13, 59)

swordD : String
swordD =
    "M 5 45 " ++
    "q -6 -17 2 -45 " ++        --left side of blade
    "q 8 28 2 45 " ++           --right side of blade
    "l 5 0 l -1 2 l -5 0 " ++   --right side of guard
    "l 0 12 l -2 0 l 0 -12 " ++ --handle
    "l -6 0 l 1 -2 l 7 0 " ++   --left side of guard
    "Z"

drawSword : Color -> Int -> Int -> Int -> Svg msg
drawSword color centerx centery radius =
    drawPathD color centerx centery radius swordSize swordD

wandSize : PathSize
wandSize =
    (13, 59)

wandD : String
wandD =
    "M 5 59 " ++
    "l 0 -45 l 2 0 l 0 45 " ++
    "M 6 15 l -8 6 l 7 -8 " ++
    "l -7 -6 l 7 4 " ++
    "l 1 -10 l 1 10 " ++
    "l 7 -4 l -7 6 " ++
    "l 7 8 l -8 -6 " ++
    "Z"

drawWand : Color -> Int -> Int -> Int -> Svg msg
drawWand color centerx centery radius =
    drawPathD color centerx centery radius wandSize wandD

towerSize : PathSize
towerSize =
    (28, 60)

towerD : String
towerD =
    "M 0 60 " ++
    "l 3 -45 l -3 -3 l 1 -12 l 5 0 l 1 7 l 4 0 l 1 -5 l 4 0 " ++
    "l 1 5 l 4 0 l 1 -7 l 5 0 l 1 12 l -3 3 l 3 45 " ++
    "Z"

drawTower : Color -> Int -> Int -> Int -> Svg msg
drawTower color centerx centery radius =
    drawPathD color centerx centery radius towerSize towerD

moonSize : PathSize
moonSize =
    (30, 60)

moonD : String
moonD =
    "M 40 50 " ++
    "A 30,30 0 1,1 40 10" ++
    "A 25,25 0 1,0 40 50"

drawMoon : Color -> Int -> Int -> Int -> Svg msg
drawMoon color centerx centery radius =
    drawPathD color centerx centery radius moonSize moonD

mageSize : PathSize
mageSize =
    (45, 60)

mageD : String
mageD =
    "M 0 60 " ++
    "l 1 -4 l 7 0 l 15 -56 l 8 2 l -4 2 " ++
    "l 13 52 l 7 0 l -1 4 " ++
    "Z"

drawMage : Color -> Int -> Int -> Int -> Svg msg
drawMage color centerx centery radius =
    drawPathD color centerx centery radius mageSize mageD

pieceBody : Piece -> Color -> Int -> Int -> Int -> Svg msg
pieceBody piece color centerx centery radius =
    case piece of
        CupPiece ->
            drawCup color centerx centery radius
        HandPiece ->
            drawHand color centerx centery radius
        SwordPiece ->
            drawSword color centerx centery radius
        WandPiece ->
            drawWand color centerx centery radius
        TowerPiece ->
            drawTower color centerx centery radius
        MoonPiece ->
            drawMoon color centerx centery radius
        MagePiece ->
            drawMage color centerx centery radius

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
    
