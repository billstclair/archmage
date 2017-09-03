----------------------------------------------------------------------
--
-- Board.elm
-- Archmage game board.
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

module Archmage.Board exposing ( initialBoard, renderInfo, render
                               , getNode
                               , stringToBoard, boardToString
                               )

import Archmage.Types as Types exposing ( Msg(..), Board, Node
                                      , Point, RenderInfo, Mode(..)
                                      , Color(..)
                                      , zeroPoint, rowLetters
                                      , get, set
                                      )
import Dict exposing ( Dict )
import Set exposing ( Set )
import Html exposing ( Html )
import Svg exposing (Svg, svg, line, g)
import Svg.Attributes exposing ( x, y, width, height
                               , cx, cy, r
                               , x1, y1, x2, y2
                               , fill, stroke, strokeWidth, fontSize, transform
                               , fillOpacity, opacity, textAnchor, dominantBaseline
                               )
import Svg.Events exposing ( onClick )
import String
import List.Extra as LE
import Debug exposing ( log )

node : Int -> String -> Int -> Node
node row rowLetter column =
    { name = rowLetter ++ (toString column)
    , row = row
    , column = column
    , piece = Nothing
    }

initialNodes : List Node
initialNodes =
    let indices = List.range 0 6
        rows = List.map2 (,) indices rowLetters
        cols = List.map (\(i,l) ->
                           List.map
                               (\j -> node i l j)
                               indices
                        )
                        rows
    in
        List.concat cols

initialBoard : Board
initialBoard =
    Dict.fromList <| List.map (\n -> (n.name, n)) initialNodes
        
renderInfo : Int -> RenderInfo
renderInfo cellSize =
    { cellSize = cellSize
    , locations = Dict.empty
    , mode = SetupMode
    , player = 1
    }

render : Board -> RenderInfo -> Html Msg
render board info =
    Html.text ""

getNode : String -> Board -> Maybe Node
getNode name board =
    Dict.get name board

setNode : String -> Node -> Board -> Board
setNode name node board =
    Dict.insert name node board

boardToString : Board -> String
boardToString board =
    ""

stringToBoard : String -> Board
stringToBoard string =
    initialBoard
