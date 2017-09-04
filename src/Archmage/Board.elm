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
                               , whiteSetupBoard, blackSetupBoard
                               , initialCaptureBoard
                               , getNode
                               , stringToBoard, boardToString
                               )

import Archmage.Types as Types
    exposing ( Msg(..), Board, Node
             , Point, PointDict, RenderInfo, Mode(..)
             , Color(..), Piece(..), NodeMsg
             , pieceList, pieceToAbbreviation, abbreviationToPiece
             , zeroPoint, rowLetters
             , get, set
             )

import Archmage.Pieces exposing ( drawPiece )

import Dict exposing ( Dict )
import Set exposing ( Set )
import Html exposing ( Html )
import Svg exposing ( Svg, svg, line, g, rect )
import Svg.Attributes exposing ( x, y, width, height
                               , cx, cy, r
                               , x1, y1, x2, y2
                               , fill, stroke, strokeWidth, fontSize, transform
                               , fillOpacity, opacity, textAnchor, dominantBaseline
                               , transform
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

indices : List Int
indices =
    List.range 0 6

boardList : List (Int, String, Int)
boardList =
    let rows = List.map2 (,) indices rowLetters
        cols = List.map (\(i, l) ->
                           List.map (\j -> (i, l, j)) indices
                        )
                        rows
    in
        List.concat cols

initialNodes : List Node
initialNodes =
    List.map (\(i, l, j) -> node i l j) boardList

initialBoard : Board
initialBoard =
    let n = Dict.fromList <| List.map (\n -> (n.name, n)) initialNodes
        nodes = case Dict.get "D3" n of
                    Nothing ->
                        n
                    Just node ->
                        Dict.insert "D3"
                            { node | piece = Just (Black, CenterHolePiece) }
                            n
    in
        { rows = 7
        , cols = 7
        , nodes = nodes
        }

setupList : List (Int, String, Piece)
setupList =
    List.map2 (\j p -> (j, pieceToAbbreviation p, p)) indices pieceList

makeSetupBoard : Color -> Board
makeSetupBoard color =
    let nodes = Dict.fromList
                <| List.map (\(j, a, p) -> (a, { name = a
                                               , row = 0
                                               , column = j
                                               , piece = Just (color, p)
                                               }
                                           )
                            )
                            setupList
    in
        { rows = 1
        , cols = 7
        , nodes = nodes
        }

whiteSetupBoard : Board
whiteSetupBoard =
    makeSetupBoard White

blackSetupBoard : Board
blackSetupBoard =
    makeSetupBoard Black

captureList : List (String, Int)
captureList =
    List.map (\(j, a, p) ->
                 [ (a ++ "1", 2 * j)
                 , (a ++ "2", (2 * j) + 1)
                 ]
             )
             setupList
        |> List.concat

initialCaptureBoard : Board
initialCaptureBoard =
    let nodes = Dict.fromList
                <| List.map (\(name, j) ->
                              (name, { name = name
                                     , row = 0
                                     , column = j
                                     , piece = Nothing
                                     }
                              )
                            )
                            captureList
    in
        { rows = 1
        , cols = 14
        , nodes = nodes
        }
        
renderInfo : Int -> RenderInfo
renderInfo cellSize =
    let setupCellSize = cellSize // 2
        locations = Dict.fromList
                    <| List.map (\n -> (n.name, { x = n.column * cellSize
                                                , y = n.row * cellSize
                                                }
                                       )
                                )
                                initialNodes
        setupLocations = Dict.fromList
                         <| List.map (\(name, node) ->
                                         (name, { x = node.column * setupCellSize
                                                , y = 0
                                                }
                                         )
                                     )
                                     (Dict.toList whiteSetupBoard.nodes)
        captureLocations = Dict.fromList
                           <| List.map (\(name, column) ->
                                         (name, { x = column * setupCellSize
                                                , y = 0
                                                }
                                         )
                                     )
                                     captureList
    in
        { cellSize = cellSize
        , locations = locations            
        , setupCellSize = setupCellSize
        , setupLineLocations = setupLocations
        , captureLineLocations = captureLocations
        }

getNode : String -> Board -> Maybe Node
getNode name board =
    Dict.get name board.nodes

setNode : String -> Node -> Board -> Board
setNode name node board =
    { board
        | nodes = Dict.insert name node board.nodes
    }

boardToString : Board -> String
boardToString board =
    ""

stringToBoard : String -> Board
stringToBoard string =
    initialBoard

maxLocation : PointDict -> (Int, Int)
maxLocation locations =
    Dict.foldl (\k {x, y} (mx, my) ->
                   (max x mx, max y my)
               )
               (0, 0)
               locations

svgLine : Int -> Int -> Int -> Int -> Svg msg
svgLine i1 j1 i2 j2 =
    line [ x1 <| toString i1
         , y1 <| toString j1
         , x2 <| toString i2
         , y2 <| toString j2
         ]
         []

gridLines : Int -> Int -> Int -> List (Svg msg)
gridLines rows cols cellSize =
    let rowLen = cols * cellSize
        colLen = rows * cellSize
    in
        List.concat
            [ List.map (\row ->
                            let y = row * cellSize
                            in
                                svgLine 0 y rowLen y
                       )
                       <| List.range 0 rows
            , List.map (\col ->
                            let x = col * cellSize
                            in
                                svgLine x 0 x colLen
                       )
                       <| List.range 0 cols
            ]

renderNodes : Board -> PointDict -> Int -> NodeMsg -> List (Svg Msg)
renderNodes board locations cellSize nodeMsg =
    List.map (\(name, node) ->
                  case Dict.get node.name locations of
                      Nothing ->
                          g [][]
                      Just {x, y} ->
                          case node.piece of
                              Nothing ->
                                  case nodeMsg board node of
                                      Nothing ->
                                          g [][]
                                      Just msg ->
                                          clickRect x y cellSize msg
                              Just (color, piece) ->
                                  let pr = drawPiece piece color x y cellSize
                                  in
                                      case nodeMsg board node of
                                          Nothing ->
                                              pr
                                          Just msg ->
                                              g []
                                                  [ pr
                                                  , clickRect x y cellSize msg
                                                  ]
             )
             <| Dict.toList board.nodes

clickRect : Int -> Int -> Int -> Msg -> Svg Msg
clickRect i j cellSize msg =
    let size = toString cellSize
    in
        rect [ x <| toString i
             , y <| toString j
             , width size
             , height size
             , fillOpacity "0"
             , onClick msg
             ]
        []

render : Board -> PointDict -> Int -> NodeMsg -> Html Msg
render board locations cellSize nodeMsg =
    let (mx, my) = maxLocation locations
        (sx, sy) = (mx+cellSize, my+cellSize)
    in
        svg [ width <| toString (2 + (board.cols * cellSize))
            , height <| toString (2 + (board.rows * cellSize))
            , stroke "black"
            , strokeWidth "2"
            , fillOpacity "0"
            ]
            [ g [ transform "translate(1, 1)" ]
                  <| List.concat
                      [ gridLines board.rows board.cols cellSize
                      , renderNodes board locations cellSize nodeMsg
                      ]
            ]
