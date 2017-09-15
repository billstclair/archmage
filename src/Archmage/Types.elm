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
                               , TheGameState(..), NodeSelection, ColoredPiece
                               , Point, PointDict, RenderInfo, Mode(..)
                               , Color(..), Player(..), GameAnalysis, emptyAnalysis
                               , NodeMsg, ClickKind(..), WhichBoard(..)
                               , Move, MovesDict, Direction(..)
                               , Message(..)
                               , getBoardPiece, setBoardPiece
                               , otherColor, playerColor, otherPlayer
                               , pieceList, pieceToString, stringToPiece
                               , rowLetters, zeroPoint
                               , get, set, rget, butLast, adjoin
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
    | SetRestoreState String
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

pieceToString : Piece -> String
pieceToString piece =
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

pieceStringDict : Dict String Piece
pieceStringDict =
    Dict.fromList [ ("H", HandPiece)
                  , ("C", CupPiece)
                  , ("S", SwordPiece)
                  , ("W", WandPiece)
                  , ("T", TowerPiece)
                  , ("M", MoonPiece)
                  , ("G", MagePiece)
                  , ("O", CenterHolePiece)
                  ]

stringToPiece : String -> Piece
stringToPiece string =
    case Dict.get string pieceStringDict of
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

type alias GameAnalysis =
    { moves : MovesDict
    , isKo : Bool
    , noMoves : Bool
    , otherNoMoves : Bool
    , noNonKoMoves : Bool
    , otherNoNonKoMoves : Bool
    }

emptyAnalysis : GameAnalysis
emptyAnalysis =
    { moves = Dict.empty
    , isKo = False
    , noMoves = False
    , otherNoMoves = False
    , noNonKoMoves = False
    , otherNoNonKoMoves = False
    }

type TheGameState =
    TheGameState GameState

type alias GameState =
    { player : Player
    , mode : Mode
    , isFirstMove : Bool
    , actor : Maybe Node
    , subject : Maybe Node
    , board : Board
    , topList : Board
    , bottomList : Board
    , history : List String     --boardToString results
    , turnMoves : List TheGameState
    , analysis : GameAnalysis
    }

getBoardPiece : String -> Board -> Maybe ColoredPiece
getBoardPiece name board =
    case Dict.get name board.nodes of
        Nothing ->
            Nothing
        Just node ->
            node.piece

setBoardPiece : String -> Maybe ColoredPiece -> Board -> Board
setBoardPiece name piece board =
    case Dict.get name board.nodes of
        Nothing ->
            board
        Just node ->
            { board
                | nodes = Dict.insert
                  name { node | piece = piece } board.nodes
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

rget : v -> XPlist k v -> Maybe k
rget value plist =
    case plist of
        [] ->
            Nothing
        (k, v) :: rest ->
            if value == v then
                Just k
            else
                rget value rest

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

type Message
    = RawMessage String String (List (String, String))
      | NewReq { name : String
               , isPublic : Bool
               , restoreState : Maybe GameState
               }
      | NewRsp { gameid : String
               , name : String
               }
      | JoinReq { gameid : String
                , name : String
                }
      | JoinRsp { gameid : String
                , player : Player
                , name : String
                }
      -- Sent as response to most commands
      | UpdateRsp { gameid : String
                  , gameState : GameState
                  }
      | SelectPlacementReq { gameid : String
                           , piece : ColoredPiece
                           }
      | PlaceReq { gameid : String
                 , piece : ColoredPiece
                 , node : String
                 }
      | SelectActorReq { gameid : String
                       , node : String
                       }
      | SelectSubjectReq { gameid : String
                         , node : String
                         }
      | MoveReq { gameid : String
                , node : String
                }
      -- Errors
      | UndoReq { gameid : String }
      | ErrorRsp { request : String
                 , text : String
                 }
      -- Chat
      | ChatReq { gameid : String
                , player : Player
                , text : String
                }
      | ChatRsp { gameid : String
                , player : Player
                , text : String
                }
