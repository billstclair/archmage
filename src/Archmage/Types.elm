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

module Archmage.Types exposing ( Page(..), Msg(..), Piece(..), Board, Node
                               , Point, PointDict, RenderInfo, Mode(..)
                               , Color(..)
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
    | NodeClick String
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
            "L"
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
                  , ("L", CenterHolePiece)
                  ]

abbreviationToPiece : String -> Piece
abbreviationToPiece abbreviation =
    case Dict.get abbreviation abbreviationDict of
        Nothing ->
            NoPiece
        Just piece ->
            piece

type alias ColoredPiece =
  ( Color, Piece )

type alias Board =
    { rows : Int
    , cols : Int
    , nodes: Dict String Node
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

type alias Point =
    { x : Int
    , y : Int
    }

zeroPoint =
    { x = 0, y = 0 }

type Mode
    = SetupMode
    | PlayMode
    | GameOverMode

type alias PointDict =
    Dict String Point

type alias RenderInfo =
    { cellSize : Int
    , locations : PointDict
    , setupCellSize : Int
    , setupLineLocations : PointDict
    , captureLineLocations : PointDict
    }

type Color
    = Black
    | White

type alias XPlist a =
    List (String, a)

get : String -> XPlist a -> Maybe a
get key plist =
    case plist of
        [] ->
            Nothing
        (k, v) :: rest ->
            if key == k then
                Just v
            else
                get key rest

set : String -> a -> XPlist a -> XPlist a
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

