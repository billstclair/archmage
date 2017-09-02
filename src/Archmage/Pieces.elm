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

circlePiece : Color -> Int -> Int -> Int -> Svg msg
circlePiece color centerx centery radius =
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

cupPiece : Color -> Int -> Int -> Int -> Svg msg
cupPiece color centerx centery radius =
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
                " L " ++ tlx ++ " " ++ tly
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

pieceBody : Piece -> Color -> Int -> Int -> Int -> Svg msg
pieceBody piece color centerx centery radius =
    case piece of
        CupPiece ->
            cupPiece color centerx centery radius
        _ ->
            circlePiece color centerx centery radius

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
                       ]
                  []
            , pieceBody piece color centerx centery radius
            ]
    
