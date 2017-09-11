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

module Archmage.Board exposing ( initialGameState, initialBoard, renderInfo, render
                               , whiteSetupBoard, blackSetupBoard
                               , initialCaptureBoard
                               , centerHolePiece, centerHoleNode
                               , getNode, setNode
                               , stringToBoard, boardToString
                               , horizontalNeighbors, diagonalNeighbors
                               , allHorizontalNeighbors, allDiagonalNeighbors
                               , validMoves, validMovesForNode, makeMove
                               , isKo, boardIsKo
                               , pieceMoveData, namesToNodes
                               , printNode, printMove, printMoves
                               , dummyBoard
                               )

import Archmage.Types as Types
    exposing ( GameState, Msg(..), Board, Node, NodeSelection, TheGameState(..)
             , Player(..)
             , Point, PointDict, RenderInfo, Mode(..)
             , Color(..), Piece(..), ColoredPiece, NodeMsg, Move, MovesDict
             , Direction(..)
             , getBoardPiece, setBoardPiece, otherColor
             , pieceList, pieceToString, stringToPiece
             , zeroPoint, rowLetters
             , get, set
             )

import Archmage.Pieces exposing ( drawPiece, pieceTitle )

import Dict exposing ( Dict )
import Char
import Html exposing ( Html )
import Svg exposing ( Svg, svg, line, g, rect, title, text, text_ )
import Svg.Attributes exposing ( x, y, width, height
                               , cx, cy, r
                               , x1, y1, x2, y2
                               , fill, stroke, strokeWidth, fontSize, fontWeight
                               , transform, alignmentBaseline
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

centerHolePiece : ColoredPiece
centerHolePiece =
    (Black, CenterHolePiece)

centerHoleNode : Node
centerHoleNode =
    { name = "D3"
    , row = 3
    , column = 3
    , piece = Just centerHolePiece
    }

initialBoard : Board
initialBoard =
    let n = Dict.fromList <| List.map (\n -> (n.name, n)) initialNodes
        nodes = case Dict.get "D3" n of
                    Nothing ->
                        n
                    Just node ->
                        Dict.insert "D3"
                            { node | piece = Just centerHolePiece }
                            n
    in
        { rows = 7
        , cols = 7
        , nodes = nodes
        }

setupList : List (Int, String, Piece)
setupList =
    List.map2 (\j p -> (j, pieceToString p, p)) indices pieceList

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
    List.map2 (\name n -> (name, n))
        ["A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N"]
        (List.range 0 13)

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
        
initialGameState : Bool -> GameState
initialGameState doPlaceAll =
    let res = { player = WhitePlayer
              , mode = SetupMode
              , isFirstMove = True
              , actor = Nothing
              , subject = Nothing
              , board = initialBoard
              , topList = whiteSetupBoard
              , bottomList = blackSetupBoard
              , history = []
              , turnMoves = []
              }
    in
        if not doPlaceAll then
            res
        else
            { res
                | mode = ChooseActorMode
                , board = dummyBoard
                , topList = initialCaptureBoard
                , bottomList = initialCaptureBoard
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
    Dict.get name board.nodes

setNode : String -> Node -> Board -> Board
setNode name node board =
    { board
        | nodes = Dict.insert name node board.nodes
    }

firstChar : String -> Char
firstChar string =
    String.toList string
        |> List.head
        |> Maybe.withDefault 'x'

pieceToChar : Maybe ColoredPiece -> Char
pieceToChar piece =
    case piece of
        Nothing ->
            '-'
        Just (color, piece) ->
            let letter = firstChar <| pieceToString piece
            in
                case color of
                    White ->
                        Char.toLower letter
                    Black ->
                        Char.toUpper letter

charToPiece : Char -> Maybe ColoredPiece
charToPiece char =
    let str = String.fromChar char
        ustr = String.toUpper str
    in
        if str == "-" then
            Nothing
        else
            let piece = stringToPiece ustr
                color = if str == String.toUpper str then
                            Black
                        else
                            White
            in
                Just (color, piece)

nodeLessp : Node -> Node -> Order
nodeLessp n1 n2 =
    if n1.row < n2.row then
        LT
    else if n1.row > n2.row then
        GT
    else if n1.column < n2.column then
        LT
    else if n2.column < n1.column then
        GT
    else
        EQ

boardToString : Board -> String
boardToString board =
    Dict.toList board.nodes
        |> List.map Tuple.second
        |> List.sortWith nodeLessp
        |> List.map (pieceToChar << .piece)
        |> String.fromList

setupRowColToNodeName : Int -> Int -> String
setupRowColToNodeName row col =
    case LE.find (\(j, _, _) -> j == col) setupList of
        Nothing ->
            " "                 --can't happen
        Just (_, res, _) ->
            res

captureRowColToNodeName : Int -> Int -> String
captureRowColToNodeName row col =
    case LE.find (\(_, j) -> j == col) captureList of
        Nothing ->
            " "                 --can't happen
        Just (res, _) ->
            res

mainBoardRowColToNodeName : Int -> Int -> String
mainBoardRowColToNodeName row col =
    let rowName = case LE.getAt row rowLetters of
                      Nothing ->
                          toString row --can't happen
                      Just name ->
                          name
    in
        rowName ++ (toString col)        

stringToBoard : String -> Board
stringToBoard string =
    let len = String.length string
        rows = if len <= 14 then
                   1
               else
                   7
        cols = if len == 14 then
                     14
                 else
                     7
        rowColToName = if len == 7 then
                           setupRowColToNodeName
                       else if len == 14 then
                           captureRowColToNodeName
                       else
                           mainBoardRowColToNodeName
        loop : List Char -> Int -> Int -> List (String, Node) -> Board
        loop = (\chars row col res ->
                    case chars of
                        [] ->
                            { rows = rows
                            , cols = cols
                            , nodes = Dict.fromList res
                            }
                        char :: tail ->
                            let name = rowColToName row col
                                piece = charToPiece char
                                (r, c) = if col+1 >= cols then
                                             (row+1, 0)
                                         else
                                             (row, col+1)
                            in
                                loop tail r c
                                    <| (name, { name = name
                                              , row = row
                                              , column = col
                                              , piece = piece
                                              }
                                       ) :: res
               )
    in
        loop (String.toList string) 0 0 []                            

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

selectionWidth : Int
selectionWidth =
    4

addSelectionRect : Node -> Point -> Int -> Maybe NodeSelection -> Svg Msg -> Svg Msg
addSelectionRect node loc cellSize selection svg =
    let sx = toString (loc.x + selectionWidth - 1)
        sy = toString (loc.y + selectionWidth - 1)
        size = toString (cellSize - (2 * (selectionWidth - 1)))
    in
        case selection of
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
                               , strokeWidth <| toString selectionWidth
                               , stroke color
                               , fillOpacity "0"
                               ]
                              []
                        , svg
                        ]

renderNode : Board -> Node -> Point -> Int -> Maybe NodeSelection -> NodeMsg -> Svg Msg
renderNode board node {x, y} cellSize selection nodeMsg =
    case node.piece of
        Nothing ->
            case nodeMsg board node of
                Nothing ->
                    g [][]
                Just msg ->
                    clickRect x y cellSize msg
        Just (color, piece) ->
            let inset = case selection of
                            Nothing ->
                                0
                            _ ->
                                selectionWidth
                pr = drawPiece piece color x y cellSize inset
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
                          let selection = findNodeSelection node selections
                          in
                              addSelectionRect node loc cellSize selection
                                  <| renderNode board node loc cellSize selection nodeMsg
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

renderBoardLabels : Int -> Int -> Int -> Int -> Svg Msg
renderBoardLabels labsize cellSize rows cols =
    let half = labsize // 2
        quar = labsize // 4
        shalf = toString half
        squar = toString quar
        chalf = cellSize // 2
        size = (3 * labsize // 4)
        ssize = toString size
    in
        g [ fontSize ssize
          , fontWeight "bold"
          , fill "black"
          , fillOpacity "1"
          ]
        [ g [ textAnchor "middle"
            , alignmentBaseline "baseline"
            , transform
                  <| "translate(" ++ toString (labsize + chalf)
                      ++ ", " ++ toString (7*labsize//8) ++ ")"
            ]
              ( List.map (\col ->
                              text_ [ x <| toString (col * cellSize)
                                    , y "0"
                                    ]
                              [ text <| toString (col+1) ]
                         )
                    <| List.range 0 cols
              )
        , g [ textAnchor "end"
            , alignmentBaseline "middle"
            , transform
                  <| "translate(" ++ toString (labsize*7//8)
                      ++ ", " ++ toString (labsize+chalf) ++ ")"
            ]
              ( List.map2 (\row lab ->
                              text_ [ x "0"
                                    , y <| toString (row * cellSize)
                                    ]
                              [ text lab ]
                         )
                    (List.range 0 rows)
                    <| List.take rows rowLetters
              )
        ]

render : Board -> Bool -> PointDict -> Int -> List NodeSelection -> NodeMsg -> Html Msg
render board doLabels locations cellSize selections nodeMsg =
    let (mx, my) = maxLocation locations
        (sx, sy) = (mx+cellSize, my+cellSize)
        boardWidth = 2 + (board.cols * cellSize)
        boardHeight = 2 + (board.rows * cellSize)
        labsize = if doLabels then (cellSize // 4) else 0
        slabsize = toString labsize
        totalWidth = boardWidth + labsize + labsize
        totalHeight = boardHeight + labsize
    in
        svg [ width <| toString totalWidth
            , height <| toString totalHeight
            ]
        [ if doLabels then
              renderBoardLabels labsize cellSize board.rows board.cols
          else
              g [][]
        , g [ transform <| "translate(" ++ slabsize ++ ", " ++ slabsize ++ ")" ]
              [ svg [ width <| toString boardWidth
                    , height <| toString boardHeight
                    , stroke "black"
                    , strokeWidth "2"
                    ]
                    [ g [ transform "translate(1, 1)" ]
                          <| List.concat
                          [ gridLines board.rows board.cols cellSize
                          , renderNodes board locations cellSize selections nodeMsg
                          ]
                    ]
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
                (row, col)

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
                        , if col <= 0 then
                              []
                          else
                              [[ row ++ (toString <| col-1)
                               , row ++ (toString <| col-2)
                               ]
                              ]
                        , if rowBelow == "" then
                              []
                          else
                              let twoBelow = Maybe.withDefault " "
                                             <| LE.getAt (idx+2) rowLetters
                              in
                                  [[rowBelow ++ scol, twoBelow ++ scol]]
                        , if col > 5 then
                              []
                          else
                              [[ row ++ (toString <| col+1)
                               , row ++ (toString <| col+2)
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
                                      [ if col <= 0 then
                                            []
                                        else
                                            [[ rowAbove ++ (toString <| col-1)
                                              , twoAbove ++ (toString <| col-2)
                                             ]
                                            ]
                                      , if col > 5 then
                                            []
                                        else
                                            [[ rowAbove ++ (toString <| col+1)
                                              , twoAbove ++
                                                   (if col == 5 then
                                                        "7"
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
                                      [ if col <= 0 then
                                            []
                                        else
                                            [[ rowBelow ++ (toString <| col-1)
                                             , twoBelow ++ (toString <| col-2)
                                             ]
                                            ]
                                      , if col > 5 then
                                            []
                                        else
                                            [[ rowBelow ++ (toString <| col+1)
                                             , twoBelow ++
                                                 (if col == 5 then
                                                      "7"
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
                         if c < 0 then
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
                                 <| (r ++ (toString <| c+1))
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
                                     ( if cl < 0 then
                                           left
                                       else
                                           (rowAbove ++ (toString (cl-1))) :: left
                                     , if cr > 6 then
                                           right
                                       else
                                           (rowAbove ++ (toString (cr+1))) :: right
                                     )
                    )
        belowLoop : Int -> Int -> Int -> (List String, List String) -> (List String, List String)
        belowLoop = (\i cl cr (left, right) ->
                         if i > 6 then
                             (List.reverse left, List.reverse right)
                         else
                             let rowBelow = Maybe.withDefault " "
                                            <| LE.getAt (i+1) rowLetters
                             in
                                 belowLoop (i+1) (cl-1) (cr+1)
                                     ( if cl < 0 then
                                           left
                                       else
                                           (rowBelow ++ (toString (cl-1))) :: left
                                     , if cr > 6 then
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

boardIsKo : Board -> List String -> Bool
boardIsKo board history =
    List.member (boardToString board) history

isKo : GameState -> Bool
isKo gs =
    not gs.isFirstMove && boardIsKo gs.board gs.history

nodePiece : Maybe Node -> Maybe ColoredPiece
nodePiece node =
    case node of
        Nothing ->
            Nothing
        Just n ->
            n.piece

makeMove : String -> GameState -> GameState
makeMove targetName gs =
    let board = gs.board
        actor = nodePiece gs.actor
        subject = nodePiece gs.subject
        target = getBoardPiece targetName board
        isCapture = case target of
                        Nothing ->
                            False
                        Just (_, p) ->
                            p == CenterHolePiece
    in
        if actor == Nothing || subject == Nothing ||
            (target /= Nothing && not isCapture)
        then
            gs
        else
            let b2 = case actor of
                         Nothing ->
                             board --can't happen
                         Just (c, p) ->
                             case gs.actor of
                                 Nothing ->
                                     board --can't happen
                                 Just {name} ->
                                     setBoardPiece
                                         name (Just (otherColor c, p)) board
                b3 = case gs.subject of
                         Nothing ->
                             b2 --can't happen
                         Just {name} ->
                             setBoardPiece name Nothing b2
                gs3 = if not isCapture then
                          { gs | board = setBoardPiece targetName subject b3 }
                      else
                          let gs2 = { gs
                                        | board = b3
                                        --There is one less piece now.
                                        --We can no longer match the old history.
                                        , history = []
                                    }
                          in
                              case subject of
                                  Nothing ->
                                      gs2 --can't happen
                                  Just piece ->
                                      doMoveCapture piece gs2
            in
                { gs3
                    | mode = ChooseActorMode
                    , isFirstMove = False
                    , subject = Nothing
                    , turnMoves = TheGameState { gs | mode = ChooseActorMode }
                                  :: gs.turnMoves
                }

doMoveCapture : ColoredPiece -> GameState -> GameState
doMoveCapture coloredPiece gs =
    let (color, piece) = coloredPiece
        board = case color of
                    Black -> gs.topList
                    White -> gs.bottomList
        name = case LE.find (\(_, node) -> node.piece == Nothing)
                       <| Dict.toList board.nodes
               of
                   Nothing ->
                       ""       --can't happen
                   Just (_, node) ->
                       node.name
        newBoard = setBoardPiece name (Just coloredPiece) board
    in
        case color of
            Black ->
                { gs | topList = newBoard }
            White ->
                { gs | bottomList = newBoard }

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
                             Nothing -> centerHolePiece
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

namesToNodes : Dict String Node -> List String -> List Node
namesToNodes nodeDict names =
    List.map (\name ->
                  Maybe.withDefault centerHoleNode
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
                                        Just (_, CenterHolePiece) ->
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
    let loop = (\tail maybeTarget ->
                    case tail of
                        [] ->
                            Nothing
                        [subject] ->
                            case maybeTarget of
                                Nothing ->
                                    Nothing
                                Just target ->
                                    maybeRes subject target
                        subject :: tail ->
                            case subject.piece of
                                Nothing ->
                                    loop tail (Just subject)
                                Just (_, CenterHolePiece) ->
                                    loop tail (Just subject)
                                Just _ ->
                                    case maybeTarget of
                                        Nothing ->
                                            Nothing
                                        Just target ->
                                            maybeRes subject target
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
        loop nodes Nothing

---
--- An initial board position for play in elm repl
---

dummyPlacements : List (String, ColoredPiece)
dummyPlacements =
    [ ("A1", (White, HandPiece))
    , ("A2", (Black, HandPiece))
    , ("A4", (White, CupPiece))
    , ("A6", (Black, CupPiece))
    , ("C1", (White, SwordPiece))
    , ("C2", (Black, SwordPiece))
    , ("B4", (White, WandPiece))
    , ("B6", (Black, WandPiece))
    , ("D1", (White, TowerPiece))
    , ("D2", (Black, TowerPiece))
    , ("D6", (White, MoonPiece))
    , ("E2", (Black, MoonPiece))
    , ("F1", (White, MagePiece))
    , ("F2", (Black, MagePiece))
    ]

dummyBoard : Board
dummyBoard =
    List.foldl (\(pos, piece) board ->
                  setBoardPiece pos (Just piece) board
               )
               initialBoard
               dummyPlacements
