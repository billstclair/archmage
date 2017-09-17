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
             , Message(..), ServerInterface(..)
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
            case checkOnlyGameid state message gameid of
                Err err ->
                    (state, err)
                Ok gameState ->
                    updateGameStateFromResult gameid message state
                        <| undoReq gameState
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
            updateGameStateFromResult gameid message state
                        <| playFun gameState

updateGameStateFromResult : String -> Message -> ServerState -> Result String GameState -> (ServerState, Message)
updateGameStateFromResult gameid message state result =
    case result of
        Err msg ->
            ( state, errorRsp message msg )
        Ok gs ->
            let message = UpdateRsp { gameid = gameid
                                    , gameState = gs
                                    }
            in
                updateGameState gameid state (gs, message)

updateGameState : String -> ServerState -> (GameState, Message) -> (ServerState, Message)
updateGameState gameid state (gameState, message) =
    ( { state
          | gameDict = Dict.insert gameid gameState state.gameDict
      }
    , message
    )

joinReq : ServerState -> GameState -> Message -> String -> String -> (ServerState, Message)
joinReq state gameState message gameid name =
    let msg = JoinRsp { gameid = gameid
                      , player = BlackPlayer
                      , name = name
                      }
        st2 = { state
                  | publicGames =
                      removeGameFromList state.publicGames gameid
              }
    in
        (st2, msg)

selectPlacementReq : GameState -> String -> Result String GameState
selectPlacementReq state node =
    let list = case state.player of
                   WhitePlayer ->
                       state.topList
                   BlackPlayer ->
                       state.bottomList
    in
        case Board.getNode node list of
            Nothing ->
                Err <| "Node not found: " ++ node
            Just n ->
                Ok { state | subject = Just n }

placeReq : GameState -> String -> Result String GameState
placeReq state node =
    Err "placeReq not yet implemented."

selectActorReq : GameState -> String -> Result String GameState
selectActorReq state node =
    Err "SelectActorReq not yet implemented."

selectSubjectReq : GameState -> String -> Result String GameState
selectSubjectReq state node =
    Err "SelectSubjectReq not yet implemented."

moveReq : GameState -> String -> Result String GameState
moveReq state node =
    Err "MoveReq not yet implemented."

endTurnReq : GameState -> Result String GameState
endTurnReq state =
    Err "EndTurnReq not yet implemented."

undoReq : GameState -> Result String GameState
undoReq state =
    case state.undoState of
        Nothing ->
            Err "No history"
        Just (TheGameState gs) ->
            Ok gs
