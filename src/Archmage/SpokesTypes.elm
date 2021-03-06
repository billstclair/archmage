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
                               , Point, Sizes, RenderInfo
                               , Color(..), MovedStone(..), NodeClassification(..)
                               , Move(..), Turn, History, newTurn
                               , StonePile, DisplayList
                               , Message(..), ServerPhase(..), GameOverReason(..)
                               , RemoveStoneVotes
                               , GameState, ServerState, ServerInterface(..)
                               , RestoreState
                               , PublicGames, PublicGame, emptyPublicGames
                               , zeroPoint, emptyStonePile, emptyDisplayList
                               , get, set, butLast, adjoin, toBitmap, fromBitmap
                               , movedStoneString, stringToMovedStone
                               , noMessage
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
    | SetPlayers Int
    | SetIsLocal Bool
    | SetIsGlobal Bool
    | SetName String
    | SetGameid String
    | SetServerUrl String
    | SetPlaceOnly Bool
    | SetUnresolvableVote Int Bool
    | SetRemoveStoneVote Int Bool
    | SetRestoreState String
    | NewGame
    | RestoreGame
    | JoinGame
    | JoinPublicGame String
    | ResignGame
    | SetInput Int String
    | Place
    | NodeClick String
    | PileClick StonePile
    | Focus Int
    | SetInputColor Color
    | SetPage Page
    | RefreshPublicGames
    | Undo
    | ServerResponse (ServerInterface Msg) Message
    | WebSocketMessage String
    | ReceiveServerUrl (Result Http.Error String)
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

type alias ColoredPiece =
  { color : Color
  , piece : Piece
  }

type alias Board =
    Dict String Node

type alias Node =
    { name : String
    , circle : String
    , spoke : Int
    , connections : List String
    , whiteStones : Int
    , blackStones : Int
    }

type alias Point =
    { x : Int
    , y : Int
    }

zeroPoint =
    { x = 0, y = 0 }

type alias Sizes =
    { diameter : Int
    , center : Int
    , bRadius : Int
    , cRadius : Int
    , dRadius : Int
    , radius : Int
    , stoneRadius : Int
    }

type alias RenderInfo =
    { sizes : Sizes
    , locations : Dict String Point
    , textLocations : Dict String Point
    , stoneLocations  : Dict String (Point, Point)
    , placement : Maybe Move
    , players : Maybe Int
    , playerNames : List (Int, String)
    , playerNumber : Maybe Int
    , resolver : Maybe Int
    }

type Color
    = Black
    | White

type MovedStone
    = MoveBlack
    | MoveWhite
    | MoveBlock

movedStoneString : MovedStone -> String
movedStoneString stone =
    case stone of
        MoveBlack -> "Black"
        MoveWhite -> "White"
        MoveBlock -> "Block"

stringToMovedStone : String -> Maybe MovedStone
stringToMovedStone string =
    case string of
        "Black" ->
            Just MoveBlack
        "White" ->
            Just MoveWhite
        "Block" ->
            Just MoveBlock
        _ ->
            Nothing

type NodeClassification
    = Empty
    | BlackOnly
    | WhiteOnly
    | WhiteWhite
    | BlackBlack
    | Blocked

type Move
    = Placement Color String
    | Resolution MovedStone String String

type alias Turn =
    { number : Int
    , resolver : Int
    , placements : List Move
    , resolutions : List Move
    }

type alias History =
    List Turn

newTurn : Int -> Int -> Turn
newTurn number resolver =
    { number = number
    , resolver = resolver
    , placements = []
    , resolutions = []
    }

type alias StonePile =
    { nodeName : String
    , colors : List String
    , location : Point
    , resolutions : List Move
    }

emptyStonePile : StonePile
emptyStonePile =
    { nodeName = ""
    , colors = []
    , location = zeroPoint
    , resolutions = []
    }

type alias DisplayList =
    { allPiles : List StonePile
    , unresolvedPiles : List StonePile
    }

emptyDisplayList : DisplayList
emptyDisplayList =
    { allPiles = []
    , unresolvedPiles = []
    }

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

toBitmap : List Int -> Int
toBitmap ints =
    List.foldl (\int res -> res + 2^(int-1)) 0 ints

fromBitmap : Int -> List Int
fromBitmap int =
    let loop : Int -> Int -> List Int -> List Int
        loop = (\n bit res ->
                    if n == 0 then
                        List.reverse res
                    else
                        let nextn = Bitwise.shiftRightZfBy 1 n
                        in
                            if 0 == (Bitwise.and n 1) then
                                loop nextn (bit+1) res
                            else
                                loop nextn (bit+1) (bit :: res)
               )
    in
        loop int 1 []

---
--- Backend interface
---

type GameOverReason
    = ResignationReason Int
    | UnresolvableVoteReason (List Move)
    | UnresolvableReason (List Move)
    | HomeCircleFullReason Int (List Move)
    | TimeoutReason
    | UnknownReason String

type alias PublicGame =
    { gameid : String
    , players : Int
    , playerNames : List String
    }

type alias PublicGames =
    { twoPlayer : List PublicGame
    , fourPlayer : List PublicGame
    }

emptyPublicGames : PublicGames
emptyPublicGames = { twoPlayer = []
                   , fourPlayer = []
                   }

noMessage : Message
noMessage =
    RawMessage "" "" []

type Message
    = RawMessage String String (List (String, String))
    -- Basic game play
    | NewReq { players : Int
             , name : String
             , isPublic : Bool
             , restoreState : Maybe RestoreState
             }
    | NewRsp { gameid : String
             , players : Int
             , name : String
             , playerid : String
             , restoreState : Maybe RestoreState
             , reason : Maybe GameOverReason
             }
    | JoinReq { gameid : String, name : String }
    | JoinRsp { gameid : String
              , players : Int
              , name : String
              , playerid: Maybe String
              , number : Int
              , restoreState : Maybe RestoreState
              }
    | PlaceReq { playerid : String, placement : Move }
    | PlaceRsp { gameid : String, number : Int }
    | PlacedRsp { gameid : String, placements : List Move }
    | ResolveReq { playerid : String, resolution : Move }
    | ResolveRsp { gameid : String, resolution : Move }
    | ResponseCountReq { playerid : String, number : Int }
    | ResponseCountRsp { gameid : String
                       , number : Int
                       , restoreState : RestoreState
                       , votedUnresolvable : List Int
                       }
    -- End of game
    | ResignReq { playerid : String }
    | ResignRsp { gameid : String, number: Int, placements : Maybe (List Move) }
    | UnresolvableVoteReq { playerid : String, vote : Bool }
    | UnresolvableVoteRsp { gameid : String, number : Int, vote : Bool }
    | RemoveStoneVoteReq { playerid : String
                         , resolution : Move
                         , vote : Bool
                         }
    | RemoveStoneVoteRsp { gameid : String
                         , number : Int
                         , resolution : Move
                         , vote : Bool
                         }
    | GameOverRsp { gameid : String, reason: GameOverReason }
    -- Public games
    | GamesReq
    | GamesRsp PublicGames
    -- Errors
    | UndoReq { playerid : String, message: Message }
    | UndoRsp { gameid : String, message: Message }
    | ErrorRsp { request : String, id : Int, text : String }
    -- Chat
    | ChatReq { playerid : String, text : String }
    | ChatRsp { gameid : String, text : String, number : Int }

type ServerPhase
    = StartPhase
    | JoinPhase
    | PlacementPhase
    | ResolutionPhase
    | ResignedPhase
    | GameOverPhase GameOverReason

type alias PlayerInfo =
    { gameid : String
    , number : Int
    , name : String
    , responseCount : Int
    }

type alias ServerState =
    { playerInfoDict : Dict String PlayerInfo --playerid -> PlayerInfo
    , playeridDict : Dict String (List String) -- gameid -> List playerid
    , gameDict : Dict String GameState --gameid -> GameState
    , placeOnly : Bool
    , publicGames : PublicGames
    }

type alias RemoveStoneVotes =
    { resolution : Move
    , players : List Int
    }

type alias GameState =
    { board : Board
    , restoreState : Maybe RestoreState
    , renderInfo : RenderInfo
    , phase : ServerPhase
    , unresolvedPiles : List StonePile
    , players : Int
    , resignedPlayers : List Int
    , votedUnresolvable : List Int
    , removeStoneVotes : Maybe RemoveStoneVotes
    , turn : Int
    , resolver : Int
    , placements : Dict Int Move
    , gameid : String
    , history : History
    }

type ServerInterface msg
    = ServerInterface
      { server : String
      , wrapper : ServerInterface msg -> Message -> msg
      , state : Maybe ServerState
      , sender : ServerInterface msg -> Message -> Cmd msg
      , placeOnly : Bool
      }

type alias RestoreState =
    { board : String
    , players : List String
    , resolver : Int
    }
