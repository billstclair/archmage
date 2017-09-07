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
                               , horizontalNeighbors, diagonalNeighbors
                               , allHorizontalNeighbors, allDiagonalNeighbors
                               , validMoves, validMovesForNode
                               , pieceMoveData, namesToNodes
                               , printNode, printMove, printMoves
                               , dummyBoard
                               )

import Archmage.Types as Types
    exposing ( Msg(..), Board, Node, NodeSelection
             , Point, PointDict, RenderInfo, Mode(..)
             , Color(..), Piece(..), ColoredPiece, NodeMsg, Move, MovesDict
             , Direction(..)
             , setBoardPiece
             , pieceList, pieceToAbbreviation, abbreviationToPiece
             , zeroPoint, rowLetters
             , get, set
             )

import Archmage.Pieces exposing ( drawPiece, pieceTitle )

import Dict exposing ( Dict )
import Set exposing ( Set )
import Html exposing ( Html )
import Svg exposing ( Svg, svg, line, g, rect, title, text )
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
    let setupCellSize = (cellSize * 2) // 3
        captureCellSize = cellSize // 2
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
                                         (name, { x = column * captureCellSize
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
        , captureCellSize = captureCellSize
        , captureLineLocations = captureLocations
        }

getNode : String -> Board -> Maybe Node
getNode name board =
    Dict.get (String.toUpper name) board.nodes

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

findNodeSelection : Node -> List NodeSelection -> Maybe NodeSelection
findNodeSelection node nodeSelections =
    LE.find (\(_, n) -> node.name == n) nodeSelections

nodeTitle : Node -> String
nodeTitle node =
    case node.piece of
        Nothing ->
            ""
        Just (_, piece) ->
            pieceTitle piece

addSelectionRect : Node -> Point -> Int -> List NodeSelection -> Svg Msg -> Svg Msg
addSelectionRect node loc cellSize selections svg =
    let sx = toString (loc.x + 3)
        sy = toString (loc.y + 3)
        size = toString (cellSize - 6)
    in
        case findNodeSelection node selections of
            Nothing ->
                svg
            Just (color, _) ->
                let titleString = nodeTitle node
                in
                    g []
                        [ title [] [ text titleString ]
                        , rect [ x sx
                               , y sy
                               , width size
                               , height size
                               , strokeWidth "4"
                               , stroke color
                               , fillOpacity "0"
                               ]
                              []
                        , svg
                        ]

renderNode : Board -> Node -> Point -> Int -> NodeMsg -> Svg Msg
renderNode board node {x, y} cellSize nodeMsg =
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
                        let titleString = nodeTitle node
                        in
                            g []
                                [ title [] [ text titleString ]
                                , pr
                                , clickRect x y cellSize msg
                                ]

renderNodes : Board -> PointDict -> Int -> List NodeSelection -> NodeMsg -> List (Svg Msg)
renderNodes board locations cellSize selections nodeMsg =
    List.map (\(name, node) ->
                  case Dict.get node.name locations of
                      Nothing ->
                          g [][]
                      Just loc ->
                          addSelectionRect node loc cellSize selections
                              <| renderNode board node loc cellSize nodeMsg
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

render : Board -> PointDict -> Int -> List NodeSelection -> NodeMsg -> Html Msg
render board locations cellSize selections nodeMsg =
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
                      , renderNodes board locations cellSize selections nodeMsg
                      ]
            ]

---
--- Move support
---

nodeNameComponents : String -> (String, Int)
nodeNameComponents name =
    let row = String.left 1 name
    in
        case String.toInt <| String.dropLeft 1 name of
            Err _ ->
                ("", 0)
            Ok col ->
                (String.toUpper row, col)

-- Turn a board node name into a list of horizontal neighbor names and the node
-- that one would push to.
horizontalNeighbors : String -> List (List String)
horizontalNeighbors name =
    let (row, col) = nodeNameComponents name
        scol = toString col
    in
        case LE.elemIndex row rowLetters of
            Nothing ->
                []
            Just idx ->
                let rowAbove = Maybe.withDefault ""
                               <| LE.getAt (idx-1) rowLetters
                    rowBelow = Maybe.withDefault ""
                                        <| LE.getAt (idx+1) rowLetters
                in
                    List.concat
                        [ if rowAbove == "" then
                              []
                          else
                              let twoAbove = Maybe.withDefault " "
                                             <| LE.getAt (idx-2) rowLetters
                              in
                                  [[rowAbove ++ scol, twoAbove ++ scol]]
                        , if col <= 1 then
                              []
                          else
                              [[ row ++ (toString <| col-1)
                               , row ++ (if col == 2 then
                                             "0"
                                         else
                                             (toString <| col-2)
                                        )
                               ]
                              ]
                        , if rowBelow == "" then
                              []
                          else
                              let twoBelow = Maybe.withDefault " "
                                             <| LE.getAt (idx+2) rowLetters
                              in
                                  [[rowBelow ++ scol, twoBelow ++ scol]]
                        , if col >= 7 then
                              []
                          else
                              [[ row ++ (toString <| col+1)
                               , row ++ (toString <| if col == 6 then 0 else col+2)
                               ]
                              ]
                        ]

diagonalNeighbors : String -> List (List String)
diagonalNeighbors name =
    let (row, col) = nodeNameComponents name
        scol = toString col
    in
        case LE.elemIndex row rowLetters of
            Nothing ->
                []
            Just idx ->
                let rowAbove = Maybe.withDefault ""
                               <| LE.getAt (idx-1) rowLetters
                    rowBelow = Maybe.withDefault ""
                                        <| LE.getAt (idx+1) rowLetters
                in
                    List.concat
                        [ if rowAbove == "" then
                              []
                          else
                              let twoAbove = Maybe.withDefault " "
                                             <| LE.getAt (idx-2) rowLetters
                              in
                                  List.concat
                                      [ if col <= 1 then
                                            []
                                        else
                                            [[ rowAbove ++ (toString <| col-1)
                                              , twoAbove ++ (toString <| col-2)
                                             ]
                                            ]
                                      , if col >= 7 then
                                            []
                                        else
                                            [[ rowAbove ++ (toString <| col+1)
                                              , twoAbove ++
                                                   (if col == 6 then
                                                        "0"
                                                    else (toString <| col+2)
                                                   )
                                             ]
                                            ]
                                      ]
                        , if rowBelow == "" then
                              []
                          else
                              let twoBelow = Maybe.withDefault " "
                                             <| LE.getAt (idx+2) rowLetters
                              in
                                  List.concat
                                      [ if col <= 1 then
                                            []
                                        else
                                            [[ rowBelow ++ (toString <| col-1)
                                             , twoBelow ++ (toString <| col-2)
                                             ]
                                            ]
                                      , if col >= 7 then
                                            []
                                        else
                                            [[ rowBelow ++ (toString <| col+1)
                                             , twoBelow ++
                                                 (if col == 6 then
                                                      "0"
                                                  else
                                                      (toString <| col+2)
                                                 )
                                             ]
                                            ]
                                      ]
                        ]

allHorizontalNeighbors : String -> List (List String)
allHorizontalNeighbors name =
    let (row, col) = nodeNameComponents name
        scol = toString col
        ensureCdr = (\l ->
                         if (List.drop 1 l) == [] then
                             []
                         else
                             [l]
                    )
        aboveLoop : Int -> String -> List String -> List String
        aboveLoop = (\i sc res ->
                         if i < 0 then
                             List.reverse res
                         else
                             let rowAbove = Maybe.withDefault " "
                                            <| LE.getAt (i-1) rowLetters
                             in
                                 aboveLoop (i-1) sc
                                     <| (rowAbove ++ sc) :: res
                    )
        belowLoop : Int -> String -> List String -> List String
        belowLoop = (\i sc res ->
                         if i >= 7 then
                             List.reverse res
                         else
                             let rowBelow = Maybe.withDefault " "
                                            <| LE.getAt (i+1) rowLetters
                             in
                                 belowLoop (i+1) sc
                                     <| (rowBelow ++ sc) :: res
                    )
        leftLoop  : String -> Int -> List String -> List String
        leftLoop  = (\r c res ->
                         if c <= 0 then
                             List.reverse res
                         else
                             leftLoop r (c-1)
                                 <| (r ++ (toString <| c-1)) :: res
                    )
        rightLoop : String -> Int -> List String -> List String
        rightLoop = (\r c res ->
                         if c >= 7 then
                             List.reverse res
                         else
                             rightLoop r (c+1)
                                 <| (r ++ (if c == 6 then "0" else (toString <| c+1)))
                                    :: res
                    )
    in
        case LE.elemIndex row rowLetters of
            Nothing ->
                []
            Just idx ->
                List.concat
                    [ ensureCdr <| aboveLoop idx scol []
                    , ensureCdr <| leftLoop row col []
                    , ensureCdr <| belowLoop idx scol []
                    , ensureCdr <| rightLoop row col []
                    ]
                    
allDiagonalNeighbors : String -> List (List String)
allDiagonalNeighbors name =
    let (row, col) = nodeNameComponents name
        scol = toString col
        ensureCdr = (\l ->
                         if (List.drop 1 l) == [] then
                             []
                         else
                             [l]
                    )
        aboveLoop : Int -> Int -> Int -> (List String, List String) -> (List String, List String)
        aboveLoop = (\i cl cr (left, right) ->
                         if i < 0 then
                             (List.reverse left, List.reverse right)
                         else
                             let rowAbove = Maybe.withDefault " "
                                            <| LE.getAt (i-1) rowLetters
                             in
                                 aboveLoop (i-1) (cl-1) (cr+1)
                                     ( if cl < 1 then
                                           left
                                       else
                                           (rowAbove ++ (toString (cl-1))) :: left
                                     , if cr > 7 then
                                           right
                                       else
                                           (rowAbove ++ (toString (cr+1))) :: right
                                     )
                    )
        belowLoop : Int -> Int -> Int -> (List String, List String) -> (List String, List String)
        belowLoop = (\i cl cr (left, right) ->
                         if i > 7 then
                             (List.reverse left, List.reverse right)
                         else
                             let rowBelow = Maybe.withDefault " "
                                            <| LE.getAt (i+1) rowLetters
                             in
                                 belowLoop (i+1) (cl-1) (cr+1)
                                     ( if cl < 1 then
                                           left
                                       else
                                           (rowBelow ++ (toString (cl-1))) :: left
                                     , if cr > 7 then
                                           right
                                       else
                                           (rowBelow ++
                                                (if cr == 7 then
                                                     "0"
                                                 else
                                                     (toString (cr+1))
                                                )
                                           ) :: right
                                     )
                    )
    in
        case LE.elemIndex row rowLetters of
            Nothing ->
                []
            Just idx ->
                let (la, ra) = aboveLoop idx col col ([], [])
                    (lb, rb) = belowLoop idx col col ([], [])
                in
                    List.concat
                        [ ensureCdr la
                        , ensureCdr ra
                        , ensureCdr lb
                        , ensureCdr rb
                        ]

allNearNeighbors : String -> List (List String)
allNearNeighbors name =
    List.concat [ horizontalNeighbors name
                , diagonalNeighbors name
                ]

validMoves : Color -> Board -> MovesDict
validMoves color board =
    Dict.values board.nodes
        |> List.concatMap (\node ->
                               let moves = validMovesForNode color board node
                               in
                                   if moves == [] then
                                       []
                                   else
                                       [(node.name, moves)]
                          )
        |> Dict.fromList

type alias PrintedNode =
    (String, Maybe ColoredPiece)

printNode : Node -> PrintedNode
printNode node =
    (node.name, node.piece)

type alias PrintedMove =
    (String, Color, Piece, String)

printMove : Move -> PrintedMove
printMove move =
    let (sn, sp) = printNode move.subject
        (tn, tp) = printNode move.target
        (color, piece) = case sp of
                             Nothing -> (Black, CenterHolePiece)
                             Just p -> p
    in
        (sn, color, piece, tn)

printMoves : MovesDict -> List (String, Color, Piece, List PrintedMove)
printMoves moves =
    Dict.values moves
        |> List.concatMap
           (\moves ->
                case moves of
                    [] ->       --can't happen
                        []
                    {actor} :: _ ->
                        [(printNode actor, (List.map printMove moves))]
           )
        |> List.map (\((name, piece), moves) ->
                       case piece of
                           Nothing ->
                               Nothing
                           Just (c, p) ->
                               Just (name, c, p, moves)
                    )
        |> LE.remove Nothing
        |> List.map (\x -> case x of
                               Nothing ->
                                 ("", Black, CenterHolePiece, [])
                               Just m ->
                                   m
                    )

pieceMoveData : Piece -> (Direction, String -> List (List String))
pieceMoveData piece =
    case piece of
        HandPiece ->
            (Push, horizontalNeighbors)
        CupPiece ->
            (Pull, horizontalNeighbors)
        SwordPiece ->
            (Push, diagonalNeighbors)
        WandPiece ->
            (Pull, diagonalNeighbors)
        TowerPiece ->
            (PushOrPull, allHorizontalNeighbors)
        MoonPiece ->
            (PushOrPull, allDiagonalNeighbors)
        MagePiece ->
            (PushOrPull, allNearNeighbors)
        _ ->
            (PushOrPull, (\s -> []))

centerNode : Node
centerNode =
    { name = "D3"
    , row = 3
    , column = 3
    , piece = Just (Black, CenterHolePiece)
    }

namesToNodes : Dict String Node -> List String -> List Node
namesToNodes nodeDict names =
    List.map (\name ->
                  Maybe.withDefault centerNode
                      <| Dict.get name nodeDict
             )
        names        
        
validMovesForNode : Color -> Board -> Node -> List Move
validMovesForNode color board actor =
    case actor.piece of
        Nothing ->
            []
        Just (nodeColor, piece) ->
            if nodeColor /= color then
                []
            else
                let (dir, getNeighbors) = pieceMoveData piece
                    neighbors = getNeighbors actor.name
                    nodeDict = board.nodes
                    neighborsNodes =
                        List.map (namesToNodes nodeDict) neighbors
                in
                    case dir of
                        Push ->
                            validPushMoves actor neighborsNodes
                        Pull ->
                            validPullMoves actor neighborsNodes
                        PushOrPull ->
                            List.concat
                                [ validPushMoves actor neighborsNodes
                                , validPullMoves actor neighborsNodes
                                ]
                    
movesLoop : Node -> List (List Node) -> (Node -> List Node -> Maybe Move) -> List Move
movesLoop actor nodeLists findMove =
    let loop = (\lists res ->
                    case lists of
                        [] ->
                            List.reverse res
                        l :: tail ->
                            case findMove actor l of
                                Nothing ->
                                    loop tail res
                                Just move ->
                                    loop tail (move :: res)
               )
    in
        loop nodeLists []

validPushMoves : Node -> List (List Node) -> List Move
validPushMoves actor nodeLists =
    movesLoop actor nodeLists findPushMove

validPullMoves : Node -> List (List Node) -> List Move
validPullMoves actor nodeLists =
    movesLoop actor nodeLists findPullMove

findPushMove : Node -> List Node -> Maybe Move
findPushMove actor nodes =
    let loop = (\tail ->
                    case tail of
                        [] ->
                            Nothing
                        [_] ->
                            Nothing
                        a :: b :: tail ->
                            case a.piece of
                                Nothing ->
                                    loop (b :: tail)
                                Just (_, CenterHolePiece) ->
                                    loop (b :: tail)
                                _ ->
                                    case b.piece of
                                        Nothing ->
                                           Just { actor = actor
                                                , subject = a
                                                , target = b
                                                }
                                        _ ->
                                            Nothing
               )
    in
        loop nodes

findPullMove : Node -> List Node -> Maybe Move
findPullMove actor nodes =
    let loop = (\tail ->
                    case tail of
                        [] ->
                            Nothing
                        [_] ->
                            Nothing
                        a :: b :: tail ->
                            case a.piece of
                                Nothing ->
                                    maybeRes b a
                                Just (_, CenterHolePiece) ->
                                    maybeRes b a
                                Just _ ->
                                    Nothing
               )
        maybeRes = (\subject target ->
                        case subject.piece of
                            Nothing ->
                                Nothing
                            Just (_, CenterHolePiece) ->
                                Nothing
                            Just _ ->
                                Just { actor = actor
                                     , subject = subject
                                     , target = target
                                     }
                   )
    in
        loop nodes

---
--- An initial board position for play in elm repl
---

dummyPlacements : List (String, ColoredPiece)
dummyPlacements =
    [ ("a1", (White, HandPiece))
    , ("a2", (Black, HandPiece))
    , ("a4", (White, CupPiece))
    , ("a6", (Black, CupPiece))
    , ("c1", (White, SwordPiece))
    , ("c2", (Black, SwordPiece))
    , ("b4", (White, WandPiece))
    , ("b6", (Black, WandPiece))
    , ("d1", (White, TowerPiece))
    , ("d2", (Black, TowerPiece))
    , ("d6", (White, MoonPiece))
    , ("d5", (Black, MoonPiece))
    , ("f1", (White, MagePiece))
    , ("f2", (Black, MagePiece))
    ]

dummyBoard : Board
dummyBoard =
    List.foldl (\(pos, piece) board ->
                  setBoardPiece pos (Just piece) board
               )
               initialBoard
               dummyPlacements
