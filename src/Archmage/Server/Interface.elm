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
        SelectPlacementReq { gameid, piece } ->
            case checkGameid state message gameid [SetupMode] of
                Err err ->
                    (state, err)
                Ok gameState ->
                    updateGameState gameid state
                        <| selectPlacementReq gameState message piece
        PlaceReq { gameid, piece, node } ->
            case checkGameid state message gameid [SetupMode] of
                Err err ->
                        (state, err)
                Ok gameState ->
                    updateGameState gameid state
                        <| placeReq gameState message piece node
        SelectActorReq { gameid, node } ->
            case checkGameid state message gameid playModes of
                Err err ->
                        (state, err)
                Ok gameState ->
                    updateGameState gameid state
                        <| selectActorReq gameState message node
        SelectSubjectReq { gameid, node } ->
            case checkGameid state message gameid playModes of
                Err err ->
                        (state, err)
                Ok gameState ->
                    updateGameState gameid state
                        <| selectSubjectReq gameState message node
        MoveReq { gameid, node } ->
            case checkGameid state message gameid playModes of
                Err err ->
                        (state, err)
                Ok gameState ->
                    updateGameState gameid state
                        <| moveReq gameState message node
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
                    updateGameState gameid state
                        <| undoReq gameid gameState message
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

selectPlacementReq : GameState -> Message -> ColoredPiece -> (GameState, Message)
selectPlacementReq state message piece =
    ( state
    , errorRsp message "SelectPlacementReq not yet implemented."
    )

placeReq : GameState -> Message -> ColoredPiece -> String -> (GameState, Message)
placeReq state message piece node =
    let foo = 1
    in
        ( state
        , errorRsp message "PlaceReq not yet implemented."
        )

selectActorReq : GameState -> Message -> String -> (GameState, Message)
selectActorReq state message node =
    ( state
    , errorRsp message "SelectActorReq not yet implemented."
    )

selectSubjectReq : GameState -> Message -> String -> (GameState, Message)
selectSubjectReq state message node =
    ( state
    , errorRsp message "SelectSubjectReq not yet implemented."
    )

moveReq : GameState -> Message -> String -> (GameState, Message)
moveReq state message node =
    ( state
    , errorRsp message "MoveReq not yet implemented."
    )

undoReq : String -> GameState -> Message -> (GameState, Message)
undoReq gameid state undoMessage =
    case state.turnMoves of
        [] ->
            ( state
            , errorRsp undoMessage "No history"
            )
        (TheGameState gs) :: tail ->
            ( { state | turnMoves = tail }
            , UpdateRsp { gameid = gameid
                        , gameState = gs
                        }
            )
