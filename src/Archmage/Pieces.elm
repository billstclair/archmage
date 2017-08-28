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

import Archmage.Types exposing ( Piece, Color )

import Svg exposing ( Svg, line )
import Svg.Attributes exposing ( x, y, width, height
                               , cx, cy, r
                               , x1, y1, x2, y2
                               , fill, stroke, strokeWidth
                               , fillOpacity, opacity
                               )

pieceSize : Float
pieceSize = 0.9

circle : Int -> Int -> Int -> Svg msg
circle centerx centery radius =
    Svg.circle [ cx (toString centerx)
               , cy (toString centery)
               , r (toString radius)
               ]
        []

drawPiece : Piece -> Color -> Int -> Int -> Int -> Svg msg
drawPiece piece color x y size =
    let centerx = x + (size // 2)
        centery = y + (size // 2)
        diameter = (toFloat size) * pieceSize
        radius = round (diameter / 2.0)
    in
        circle centerx centery radius
    
