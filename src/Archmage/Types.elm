----------------------------------------------------------------------
--
-- Types.elm
-- Shared types for Archmage game.
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

module Archmage.Types exposing ( GameState, Page(..), Msg(..), Piece(..), Board, Node
                               , NodeSelection, ColoredPiece
                               , Point, PointDict, RenderInfo, Mode(..)
                               , Color(..), Player(..)
                               , NodeMsg, ClickKind(..), WhichBoard(..)
                               , Move, MovesDict, Direction(..)
                               , setBoardPiece
                               , otherColor, playerColor, otherPlayer
                               , pieceList, pieceToAbbreviation, abbreviationToPiece
                               , rowLetters, zeroPoint
                               , get, set, butLast, adjoin
                               )

import Dict exposing ( Dict )
import Time exposing ( Time )
import Http
import Bitwise

type Page
    = GamePage
    | PublicPage
    | RulesPage
    | HelpPage

type ClickKind
    = EmptyBoardClick
    | ChooseActorClick
    | OtherPlayerClick
    | UnchooseActorClick
    | ChooseSubjectClick
    | UnchooseSubjectClick
    | ChooseTargetClick
    | SetupBoardClick

type WhichBoard
    = TopList
    | BottomList
    | MainBoard

type Msg
    = SetChatInput String
    | SendChat
    | ChatKeydown Int
    | ChatScroll Float
    | SetChatSize Int
    | SetIsLocal Bool
    | SetIsGlobal Bool
    | SetName String
    | SetGameid String
    | SetServerUrl String
    | NewGame
    | RestoreGame
    | JoinGame
    | JoinPublicGame String
    | ResignGame
    | SetInput Int String
    | NodeClick ClickKind WhichBoard Node
    | Focus Int
    | SetPage Page
    | RefreshPublicGames
    | Undo
    | Tick Time
    | Noop

type Piece
  = HandPiece
  | CupPiece
  | SwordPiece
  | WandPiece
  | TowerPiece
  | MoonPiece
  | MagePiece
  | CenterHolePiece
  | NoPiece

type Direction
    = Push
    | Pull
    | PushOrPull

pieceList : List Piece
pieceList =
    [ HandPiece, CupPiece, SwordPiece, WandPiece, TowerPiece, MoonPiece, MagePiece ]

pieceToAbbreviation : Piece -> String
pieceToAbbreviation piece =
    case piece of
        HandPiece ->
            "H"
        CupPiece ->
            "C"
        SwordPiece ->
            "S"
        WandPiece ->
            "W"
        TowerPiece ->
            "T"
        MoonPiece ->
            "M"
        MagePiece ->
            "G"
        CenterHolePiece ->
            "O"
        NoPiece ->
            ""

abbreviationDict : Dict String Piece
abbreviationDict =
    Dict.fromList [ ("H", HandPiece)
                  , ("C", CupPiece)
                  , ("S", SwordPiece)
                  , ("W", WandPiece)
                  , ("T", TowerPiece)
                  , ("M", MoonPiece)
                  , ("G", MagePiece)
                  , ("O", CenterHolePiece)
                  ]

abbreviationToPiece : String -> Piece
abbreviationToPiece abbreviation =
    case Dict.get (String.toUpper abbreviation) abbreviationDict of
        Nothing ->
            NoPiece
        Just piece ->
            piece

type alias ColoredPiece =
  ( Color, Piece )

type alias NodeSelection =
    ( String, String )

type alias Board =
    { rows : Int
    , cols : Int
    , nodes: Dict String Node
    }

type alias GameState =
    { player : Player
    , mode : Mode
    , isFirstMove : Bool
    , actor : Maybe Node
    , subject : Maybe Node
    , board : Board
    , topList : Board
    , bottomList : Board
    }

setBoardPiece : String -> Maybe ColoredPiece -> Board -> Board
setBoardPiece name piece board =
    let uname = String.toUpper name
    in
        case Dict.get uname board.nodes of
            Nothing ->
                board
            Just node ->
                { board
                    | nodes = Dict.insert
                              uname { node | piece = piece } board.nodes
                }

rowLetters : List String
rowLetters =
    [ "A", "B", "C", "D", "E", "F", "G" ]

type alias Node =
    { name : String
    , row : Int
    , column : Int
    , piece : Maybe ColoredPiece 
    }

type alias NodeMsg =
    Board -> Node -> Maybe Msg

type alias Point =
    { x : Int
    , y : Int
    }

zeroPoint =
    { x = 0, y = 0 }

type Mode
    = SetupMode
    | ChooseActorMode
    | ChooseSubjectMode
    | ChooseTargetMode
    | GameOverMode

type alias PointDict =
    Dict String Point

type alias RenderInfo =
    { cellSize : Int
    , locations : PointDict
    , setupCellSize : Int
    , setupLineLocations : PointDict
    , captureCellSize : Int
    , captureLineLocations : PointDict
    }

type Color
    = Black
    | White

otherColor : Color -> Color
otherColor color =
    case color of
        White -> Black
        Black -> White

type Player
    = WhitePlayer
    | BlackPlayer

playerColor : Player -> Color
playerColor player =
    case player of
        WhitePlayer -> White
        BlackPlayer -> Black

otherPlayer : Player -> Player
otherPlayer player =
    case player of
        WhitePlayer -> BlackPlayer
        BlackPlayer -> WhitePlayer

type alias Move =
    { actor: Node
    , subject: Node
    , target: Node
    }

type alias MovesDict =
    Dict String (List Move)

type alias XPlist k v =
    List (k, v)

get : k -> XPlist k v -> Maybe v
get key plist =
    case plist of
        [] ->
            Nothing
        (k, v) :: rest ->
            if key == k then
                Just v
            else
                get key rest

set : k -> v -> XPlist k v -> XPlist k v
set key value plist =
    (key, value) :: (List.filter (\(k,_) -> k /= key) plist)

butLast : List a -> List a
butLast list =
    let loop = (\l res ->
                    case l of
                        [_] ->
                            List.reverse res
                        head :: tail ->
                            loop tail <| head :: res
                        [] ->
                            []
               )
    in
        loop list []
                    
adjoin : a -> List a -> List a
adjoin a list =
    if List.member a list then
        list
    else
        a :: list

---
--- Backend interface
---

