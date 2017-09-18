----------------------------------------------------------------------
--
-- Interface.elm
-- JSON encoder and decoder for Archmage server wire protocol.
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

module Archmage.Server.Interface exposing ( emptyServerState
                                          , makeProxyServer, makeServer, send
                                          , getServer
                                          , processServerMessage
                                          )

import Archmage.Server.EncodeDecode exposing ( encodeMessage, modeToString )
import Archmage.Types as Types
    exposing ( GameState, Piece(..), Board, Node
             , TheGameState(..), NodeSelection, ColoredPiece
             , Mode(..), Color(..), Player(..)
             , Message(..), ServerInterface(..), PlayerNames, initialPlayerNames
             , ServerState, PublicGames, PublicGame, emptyPublicGames
             , butLast, adjoin
             )
import Archmage.Board as Board
    exposing ( initialGameState, stringToBoard, boardToString
             , isPlayMode
             )


import Dict exposing ( Dict )
import Task
import List.Extra as LE
import Debug exposing ( log )
import WebSocket

emptyServerState : ServerState
emptyServerState =
    { gameDict = Dict.empty
    , names = initialPlayerNames
    , publicGames = emptyPublicGames
    }

dummyGameid : String
dummyGameid =
    "<gameid>"

makeProxyServer : (ServerInterface msg -> Message -> msg) -> ServerInterface msg
makeProxyServer wrapper =
    ServerInterface { server = ""
                    , wrapper = wrapper
                    , state = Nothing
                    , sender = proxySender
                    }

makeServer : String -> msg -> ServerInterface msg
makeServer server msg =
    ServerInterface { server = server
                    , wrapper = (\_ _ -> msg)
                    , state = Nothing
                    , sender = sender
                    }

getServer : ServerInterface msg -> String
getServer (ServerInterface interface) =
    interface.server

send : ServerInterface msg -> Message -> Cmd msg
send ((ServerInterface interface) as si) message =
    interface.sender si message

proxyCmd : ServerInterface msg -> Message -> Cmd msg
proxyCmd ((ServerInterface interface) as si) message =
    let task = Task.succeed message
        wrapper = interface.wrapper si
    in
      Task.perform wrapper task

proxySender : ServerInterface msg -> Message -> Cmd msg
proxySender (ServerInterface interface) message =
    let state = Maybe.withDefault emptyServerState interface.state
        (s2, msgs) = processServerMessage state message
    in
        proxyCmd (ServerInterface { interface | state = Just s2 }) msgs

sender : ServerInterface msg -> Message -> Cmd msg
sender (ServerInterface interface) message =
    WebSocket.send interface.server (encodeMessage message)

errorRsp : Message -> String -> Message
errorRsp message text =
    ErrorRsp { request = encodeMessage message
             , text = text
             }

checkOnlyGameid : ServerState -> Message -> String -> Result Message GameState
checkOnlyGameid state message gameid =
    case Dict.get gameid state.gameDict of
        Just gameState ->
            Ok gameState
        Nothing ->
            Err <| errorRsp message "Unknown gameid"

checkGameid : ServerState -> Message -> String -> List Mode -> Result Message GameState
checkGameid state message gameid modes =
    case checkOnlyGameid state message gameid of
        Ok gameState as res ->
            if List.member gameState.mode modes then
                res
            else
                Err
                <| errorRsp message
                    ("Not " ++ (modeToString gameState.mode) ++ " mode")
        err ->
            err

playModes : List Mode
playModes =
    [ChooseActorMode, ChooseSubjectMode, ChooseTargetMode]

processServerMessage : ServerState -> Message -> (ServerState, Message)
processServerMessage state message =
    case message of
        -- Basic game play
        NewReq { name, isPublic, restoreState } ->
            newReq state message name isPublic restoreState
        JoinReq { gameid, name } ->
            case checkGameid state message gameid [JoinMode] of
                Err err ->
                    (state, err)
                Ok gameState ->
                    joinReq state gameState message gameid name
        SelectPlacementReq { gameid, node } ->
            doGamePlay state message gameid [SetupMode]
                (\gameState -> selectPlacementReq gameState node)
        PlaceReq { gameid, node } ->
            doGamePlay state message gameid [SetupMode]
                (\gameState -> placeReq gameState node)
        SelectActorReq { gameid, node } ->
            doGamePlay state message gameid playModes
                (\gameState -> selectActorReq gameState node)
        SelectSubjectReq { gameid, node } ->
            doGamePlay state message gameid playModes
                (\gameState -> selectSubjectReq gameState node)
        MoveReq { gameid, node } ->
            doGamePlay state message gameid playModes
                (\gameState -> moveReq gameState node)
        EndTurnReq { gameid } ->
            doGamePlay state message gameid playModes
                endTurnReq
            
        -- Public games
        GamesReq ->
            ( state
            , GamesRsp state.publicGames
            )
        -- Errors
        UndoReq { gameid } ->
            doGamePlay state message gameid playModes
                undoReq
        -- Chat
        ChatReq { gameid, player, text } ->
            case checkOnlyGameid state message gameid of
                Err err ->
                    (state, err)
                Ok gameState  ->
                    ( state
                    , ChatRsp { gameid = gameid
                              , player = player
                              , text = text
                              }
                    )
        _ ->
            ( state
            , errorRsp message "Illegal Request"
            )

appendGameList : PublicGames -> PublicGame -> PublicGames
appendGameList games game =
    List.append games [game]

removeGameFromList : PublicGames -> String -> PublicGames
removeGameFromList games gameid =
    List.filter (\game -> game.gameid /= gameid) games

maximumPublicGames : Int
maximumPublicGames =
    10

newReq : ServerState -> Message -> String -> Bool -> Maybe GameState -> (ServerState, Message)
newReq state message name isPublic restoreState =
    if isPublic then
        let list = state.publicGames
        in
            if (List.length list) >= maximumPublicGames then
                ( state
                , errorRsp message
                    "There are already too many public games."
                )
            else
                newReqInternal state message name isPublic restoreState
    else
        newReqInternal state message name isPublic restoreState

newReqInternal : ServerState -> Message -> String -> Bool -> Maybe GameState -> (ServerState, Message)
newReqInternal state message name isPublic restoreState =
    let gameState = case restoreState of
                        Nothing ->
                            initialGameState False
                        Just gs ->
                            gs
        gameid = dummyGameid
        st2 = { state
                  | gameDict =
                      Dict.insert gameid (Board.addAnalysis gameState) state.gameDict
                  , names = { initialPlayerNames | white = name }
                  , publicGames =
                    if isPublic then
                        appendGameList state.publicGames
                            <| { gameid = gameid
                               , playerName = name
                               }
                    else
                        state.publicGames                        
              }
        msg = NewRsp { gameid = gameid
                     , name = name
                     }
    in
        -- The non-proxy server will generate new gameid and playerid
        (st2, msg)

type alias PlayFun =
    GameState -> Result String GameState

doGamePlay : ServerState -> Message -> String -> List Mode -> PlayFun -> (ServerState, Message)
doGamePlay state message gameid modes playFun =
    case checkGameid state message gameid modes of
        Err err ->
            (state, err)
        Ok gameState ->
            case playFun gameState of
                Err msg ->
                    ( state, errorRsp message msg )
                Ok gs ->
                    let message = UpdateRsp { gameid = gameid
                                            , gameState = gs
                                            }
                    in
                        ( { state
                              | gameDict = Dict.insert gameid gs state.gameDict
                          }
                        , message
                        )

joinReq : ServerState -> GameState -> Message -> String -> String -> (ServerState, Message)
joinReq state gameState message gameid name =
    let names = state.names
        nms = { names | black = name }
        msg = JoinRsp { gameid = gameid
                      , names = nms
                      , gameState = gameState
                      }
        st2 = { state
                  | names = nms
                  , publicGames =
                      removeGameFromList state.publicGames gameid
              }
    in
        (st2, msg)

playerList : GameState -> Board
playerList gs =
    case gs.player of
        WhitePlayer ->
            gs.topList
        BlackPlayer ->
            gs.bottomList

setPlayerList : Board -> GameState -> GameState
setPlayerList list gs =
    case gs.player of
        WhitePlayer ->
            { gs | topList = list }
        BlackPlayer ->
            { gs | bottomList = list }

-- Click on a piece in the top or bottom rows during placement
-- Stores the node in GameState.subject
selectPlacementReq : GameState -> String -> Result String GameState
selectPlacementReq state node =
    case Board.getNode node <| playerList state of
        Nothing ->
            Err <| "Node not found: " ++ node
        Just n ->
            if n.piece == Nothing then
                Err <| "No piece at " ++ node
            else
                Ok { state | subject = Just n }

-- Click on an empty square in the board during placement
-- Move the selected piece, in GameState.subject to the main board
-- at the clicked node.
placeReq : GameState -> String -> Result String GameState
placeReq state node =
    case state.subject of
        Nothing ->
            Err "No subject selected."
        Just { name, piece } ->
            let board = state.board
            in
                if Board.getNode node board == Nothing then
                    Err <| "No such board node: " ++ node
                else
                    let list = Types.setBoardPiece name Nothing <| playerList state
                        timeToPlay = (state.player == BlackPlayer) &&
                                     (Dict.isEmpty list.nodes)
                        gs = { state
                                 | board = Types.setBoardPiece node piece board
                             }
                    in
                        if not timeToPlay then
                            Ok <| setPlayerList list
                                { gs | player = Types.otherPlayer gs.player }
                        else
                            Ok <| Board.addAnalysis
                                { gs
                                    | topList = Board.initialCaptureBoard
                                    , bottomList = Board.initialCaptureBoard
                                    , history = [boardToString gs.board]
                                    , player = WhitePlayer
                                    , isFirstMove = True
                                }

-- Click on a selected actor during game play.
-- Store its node in GameState.actor
selectActorReq : GameState -> String -> Result String GameState
selectActorReq state node =
    case state.actor of
        Just actor ->
            if actor.name == node then
                Ok { state
                       | actor = Nothing
                       , subject = Nothing
                   }
            else
                Err <| "There's already an actor selected."
        Nothing ->
            case Dict.get node state.analysis.moves of
                Nothing ->
                    Err <| "There are no valid moves for actor at: " ++ node
                Just _ ->
                    selectActorInternal state node

selectActorInternal : GameState -> String -> Result String GameState
selectActorInternal state node =
    let board = state.board
    in
        case Board.getNode node board of
            Nothing ->
                Err <| "Board has no node named: " ++ node
            Just n ->
                case n.piece of
                    Nothing ->
                        Err <| "Board has no piece at: " ++ node
                    Just (color, p) ->
                        if color /= Types.playerColor state.player then
                            Err <| "Attempt to select other player's piece as actor."
                        else
                            Ok { state | actor = Just n }

-- Click on a selected subject after choosing an actor in game play.
-- Store its node in GameState.subject
selectSubjectReq : GameState -> String -> Result String GameState
selectSubjectReq state node =
    case state.subject of
        Just subject ->
            if subject.name == node then
                Ok { state | subject = Nothing }
            else
                Err <| "There's already a subject selected."
        Nothing ->
            case state.actor of
                Nothing ->
                    Err "Attempt to choose a subject with no selected actor."
                Just { name } ->
                    case Dict.get name state.analysis.moves of
                        Nothing ->
                            -- can't happen, but need to do something here
                            Err <| "There are no valid moves for actor at: " ++ name
                        Just moves ->
                            case LE.find (\{subject} -> subject.name == node) moves of
                                Nothing ->
                                    Err <| "Not a valid subject node: " ++ node
                                Just _ ->
                                    selectSubjectInternal state node

selectSubjectInternal : GameState -> String -> Result String GameState
selectSubjectInternal state node =
    let board = state.board
    in
        case Board.getNode node board of
            Nothing ->
                Err <| "Board has no node named: " ++ node
            Just n ->
                case n.piece of
                    Nothing ->
                        Err <| "Board has no piece at: " ++ node
                    Just (color, p) ->
                        Ok { state | subject = Just n }

-- Click on a selected empty target square after selecting an actor and subject.
-- Do the move, updating GameState.board accordingly,
-- and clearing GameState.actor and GameState.subject
moveReq : GameState -> String -> Result String GameState
moveReq state node =
    case (state.actor, state.subject) of
        (Just actor, Just {name}) ->
            case Dict.get actor.name state.analysis.moves of
                Nothing ->
                    Err <| "There are no valid moves for actor at: " ++ actor.name
                Just moves ->
                    case LE.find (\{subject, target} ->
                                   name == subject.name &&
                                   node == target.name
                                 )
                                   moves
                    of
                        Nothing ->
                            Err
                            <| "There is no move with selected actor and subject to node: "
                                ++ node
                        Just _ ->
                            Ok <| Board.makeMove node state
        _ ->
            Err <| if state.actor == Nothing then
                       "No actor is selected."
                   else
                       "No subject is selected."

-- Click the "End Turn" button during game play.
-- Switch GameState.player
endTurnReq : GameState -> Result String GameState
endTurnReq state =
    let isFirstMove = state.isFirstMove
        analysis = state.analysis
        isKo = analysis.isKo
        noMoves = analysis.noMoves
        otherNoNonKoMoves = analysis.otherNoNonKoMoves
    in
        if isFirstMove && not noMoves then
            Err "You may not end your turn on the first move unless you have no moves."
        else if isKo then
            Err "You may not end your turn while in Ko."
        else if otherNoNonKoMoves then
            Err "You may not end your turn when the other player has no non-Ko moves."
        else
            let gs = { state
                         | player = Types.otherPlayer state.player
                         , isFirstMove = True
                         , mode = ChooseActorMode
                         , actor = Nothing
                         , subject = Nothing
                         , undoState = Nothing
                         , history = Board.boardToString state.board
                                     :: state.history
                     }
            in
                Ok <| Board.addAnalysis gs            

-- Click the "Undo" button during game play.
-- Revert to the saved GameState.undoState
undoReq : GameState -> Result String GameState
undoReq state =
    case state.undoState of
        Nothing ->
            Err "No history"
        Just (TheGameState gs) ->
            Ok gs
