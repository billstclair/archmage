----------------------------------------------------------------------
--
-- Archmage.elm
-- Chris St. Clair's Archmage board game.
-- The distributed version uses ArchmagePorts.elm as top-level entry point.
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

module Archmage exposing ( init, view, update, subscriptions )

import Archmage.Types as Types
    exposing ( Model
             , GameState, TheGameState(..), Piece(..), Color(..), Player(..)
             , ColoredPiece, Board, Node
             , NodeSelection, RenderInfo
             , Page(..), Msg(..), Mode(..), ClickKind(..), WhichBoard(..)
             , NodeMsg, MovesDict, PlayerNames, initialPlayerNames
             , ServerInterface, Message(..), ChatSettings
             , otherColor, playerColor, otherPlayer
             , setBoardPiece
             )
import Archmage.Pieces exposing ( drawPiece )
import Archmage.Board as Board exposing ( initialGameState, getNode, printMove
                                        , makeMove, isPlayMode
                                        , boardToString, stringToBoard
                                        , centerHoleName, centerHoleNode
                                        , pieceToChar
                                        )
import Archmage.Server.EncodeDecode
    exposing ( encodeGameState, decodeGameState, restoreGame
             , encodeMessage, decodeMessage
             )
import Archmage.Server.Interface as Interface
    exposing ( makeProxyServer, makeServer, send, dummyGameid )

import ElmChat

import Html exposing ( Html, Attribute , div, h2, text, img, p, a, button, span
                     , input
                     )
import Html.Attributes exposing ( align, src, href, target, style, disabled, title
                                , type_, size, value, width, checked
                                )
import Html.Events exposing ( onClick, onInput, onCheck )
import Char
import Dict exposing ( Dict )
import List.Extra as LE
import Task
import Window
import WebSocket
import Http
import Debug exposing ( log )

main =
    Html.program
        { init = init Nothing
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

subscriptions : Model -> Sub Msg
subscriptions model =
    if not model.isRemote then
        Window.resizes WindowSize
    else
        Sub.batch
            [ WebSocket.listen
                  (Interface.getServer model.server)
                  WebSocketMessage
            , Window.resizes WindowSize
            ]
{-

A few words about end of game.

-}

placementSelectionColor = "blue"
otherPlayerSelectionColor = "orange"
actorSelectionColor = "green"
subjectSelectionColor = "blue"
targetSelectionColor = "red"

messages : List (Mode, String)
messages =
    [ (JoinMode, "Waiting for other player to join.")
    , (SetupMode, "select and place a piece.")
    , (ChooseActorMode, "click a " ++ actorSelectionColor
           ++ " actor.")
    , (ChooseSubjectMode, "click a " ++ subjectSelectionColor
           ++ " subject or the actor.")
    , (ChooseTargetMode, "click a " ++ targetSelectionColor
           ++ " target, subject, or actor.")
    , (GameOverMode, "Game Over!")
    ]

setMessage : Model -> Model
setMessage model =
    let gs = model.gs
    in
        case Types.get gs.mode messages of
            Nothing ->
                model
            Just message ->
                let msg = if gs.mode == JoinMode then
                              if model.you == WhitePlayer then
                                  Just message
                              else
                                  Just "Waiting for server to respond."
                          else if model.you /= gs.player &&
                                  gs.mode /= GameOverMode
                          then
                              let dowhat = if gs.mode == SetupMode then
                                               "place a piece."
                                           else
                                               "finish the turn."
                              in
                                  Just <| "Wait for the other player to " ++ dowhat
                          else
                              Nothing
                in
                    case msg of
                        Nothing ->
                            setMessageInternal model gs message
                        Just msg ->
                            { model | message = Just msg }

setMessageInternal : Model -> GameState -> String -> Model
setMessageInternal model gs message =
    let c = case gs.player of
                WhitePlayer -> "White, "
                BlackPlayer -> "Black, "
        analysis = gs.analysis
        msg2 = if analysis.noNonKoMoves then
                   if analysis.otherNoNonKoMoves then
                       "Ends in Ko for both players. Undo."
                   else
                       let suffix = if gs.isFirstMove then
                                        "Click \"End Turn\"."
                                    else
                                        "Undo or End Turn."
                       in
                           "Ends in Ko. " ++ suffix
               else if gs.mode == ChooseActorMode
                   && Dict.isEmpty analysis.moves
               then
                   if analysis.otherNoNonKoMoves then
                       "other player has no non-Ko moves. Undo."
                   else if gs.isFirstMove then
                       "no moves are possible. Click \"End Turn\"."
                   else if analysis.isKo then
                       "in Ko and no moves possible. Undo."
                   else
                       "no moves possible. Undo or End Turn."
                    else
                        message
        msg = case gs.mode of
                  JoinMode ->
                      message
                  GameOverMode ->
                      if analysis.noNonKoMoves &&
                          analysis.otherNoNonKoMoves
                      then
                          "Game Over! Ends in Ko for both players."
                      else
                          message
                  _ ->
                      c ++ msg2
    in
        { model | message = Just msg }

pieceSize : Int
pieceSize =
    100

init : Maybe Model -> ( Model, Cmd Msg )
init maybeModel =
    let (model, restoreState)
        = case maybeModel of
              Just mod ->
                  ( { mod | newIsRemote = mod.isRemote }
                  , Just mod.gs)
              Nothing ->
                  let mod = { page = GamePage
                            , nodeSelections = []
                            , renderInfo = Just <| Board.renderInfo pieceSize
                            , message = Nothing
                            , restoreState = ""
                            , gs = initialGameState False
                            , isRemote = False
                            , isPublic = False
                            , server = makeProxyServer ServerMessage
                            , gameid = ""
                            , playerid = ""
                            , you = WhitePlayer
                            , names = initialPlayerNames
                            , windowSize = Nothing
                            , newIsRemote = True
                            , newGameid = ""
                            , otherPlayerid = ""
                            , chatSettings = Nothing
                            }
                  in
                      ( mod, Nothing )
        cmd = send model.server
              <| if restoreState == Nothing || not model.isRemote then
                     NewReq { name = log "NewReq" model.names.white
                            , isPublic = False
                            , restoreState = restoreState
                            }
                 else
                     UpdateReq { playerid = log "UpdateReq" model.playerid }
    in
        ( model
        , Cmd.batch
            [ cmd
            , windowSizeCmd
            ]
        )

windowSizeCmd : Cmd Msg
windowSizeCmd =
    Task.perform (\x -> WindowSize x) Window.size

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let (mod, cmd) = updateInternal msg { model | message = Nothing }
        mod2 = if mod.message == Nothing then
                   setMessage mod
               else
                   mod
    in
        (mod2, cmd)
        
getPlayerid : Model -> String
getPlayerid model =
    if model.isRemote || model.gs.player == WhitePlayer then
        model.playerid
    else
        model.otherPlayerid

connect : String -> Model -> (Model, Cmd Msg)
connect rawUrl model =
    let url = String.trim rawUrl
        server = makeServer url Noop
        rsres = case model.restoreState of
                    "" ->
                        Ok Nothing
                    json ->
                        case decodeGameState json of
                            Err msg ->
                                Err msg
                            Ok rs ->
                                Ok <| Just rs
        gs = initialGameState False
    in
        case rsres of
            Err msg ->
                ( { model | message = Just msg }
                , Cmd.none
                )
            Ok rs ->
                ( { model
                      | isRemote = True
                      , you = WhitePlayer
                      , server = server
                      , message = Just <| "Connecting to " ++ url
                      , gs = { gs | mode = JoinMode }
                  }
                , send server
                    <| NewReq { name = model.names.white
                              , isPublic = model.isPublic
                              , restoreState = rs
                              }
                )

join : String -> Model -> (Model, Cmd Msg)
join rawUrl model =
    let url = String.trim rawUrl
        server = makeServer url Noop
        gs = initialGameState False
    in
        ( { model
              | isRemote = True
              , server = server
              , gameid = model.newGameid
              , message = Just <| "Joining " ++ url
              , gs = { gs | mode = JoinMode }
              , you = BlackPlayer
          }
        , send server
            <| JoinReq { gameid = model.newGameid
                       , name = model.names.black
                       }
        )

getServerText : Http.Request String
getServerText =
    Http.getString "server.txt"

updateInternal : Msg -> Model -> ( Model, Cmd Msg )
updateInternal msg model =
    case msg of
        NewGame ->
            if model.newIsRemote then
                ( model
                , Http.send (ReceiveServerUrl connect) getServerText
                )
            else
                let (model, cmd) = init Nothing
                in
                    ( { model | newIsRemote = model.isRemote }
                    , cmd
                    )
        ReceiveServerUrl handler result ->
            case result of
                Err msg ->
                    ( { model | message = Just (toString msg) }
                    , Cmd.none
                    )
                Ok url ->
                    handler url model
        SetGameid gameid ->
            ( { model | newGameid = gameid }
            , Cmd.none
            )
        JoinGame ->
            ( model
            , Http.send (ReceiveServerUrl join) getServerText
            )
        ServerMessage si message ->
            serverMessage si message model
        WebSocketMessage string ->
            case decodeMessage string of
                Err err ->
                    ( { model | message = Just err }
                    , Cmd.none
                    )
                Ok message ->
                    serverMessage model.server message model
        ChatUpdate settings cmd ->
            ( { model | chatSettings = Just settings }
            , cmd
            )
        ChatSend line settings ->
            -- This moves to the server message processing
            ( { model | chatSettings = Just settings }
            , send model.server
                <| ChatReq { playerid = model.playerid
                           , text = line
                           }
            )
        SetIsRemote isRemote ->
            ( { model | newIsRemote = isRemote }
            , Cmd.none
            )
        SetIsPublic isPublic ->
            ( { model | isPublic = isPublic }
            , Cmd.none
            )
        SetRestoreState text ->
            ( { model | restoreState = text }
            , Cmd.none
            )
        RestoreGame ->
            case decodeGameState model.restoreState of
                Err msg ->
                    ( { model | message = Just msg }
                    , Cmd.none
                    )
                Ok gs ->
                    ( model
                    , send model.server
                        <| NewReq { name = model.names.white
                                  , isPublic = False
                                  , restoreState = Just gs
                                  }
                    )
        SetPage page ->
            ( { model | page = page }
            , Cmd.none
            )
        Undo ->
            ( model
            , send model.server
                <| UndoReq { playerid = getPlayerid model }
            )
        NodeClick kind which node ->
            case kind of
                SetupBoardClick ->
                    ( model
                    , send model.server
                        <| SelectPlacementReq { playerid = getPlayerid model
                                              , node = node.name
                                              }
                    )
                EmptyBoardClick ->
                    ( model
                    , send model.server
                        <| PlaceReq { playerid = getPlayerid model
                                    , node = node.name
                                    }
                    )
                OtherPlayerClick ->
                    ( model
                    , send model.server
                        <| EndTurnReq { playerid = getPlayerid model }
                    )
                ChooseActorClick ->
                    ( model
                    , send model.server
                        <| SelectActorReq { playerid = getPlayerid model
                                          , node = node.name
                                          }
                    )
                ChooseSubjectClick ->
                    ( model
                    , send model.server
                        <| SelectSubjectReq { playerid = getPlayerid model
                                            , node = node.name
                                            }
                    )
                ChooseTargetClick ->
                    ( model
                    , send model.server
                        <| MoveReq { playerid = getPlayerid model
                                   , node = node.name
                                   }
                    )
        WindowSize size ->
            ( { model
                  | windowSize = Just size
                  , renderInfo = Just <| Board.renderInfo <| windowSizeToPieceSize size
              }
            , Cmd.none
            )
        _ ->
            ( model, Cmd.none )

-- height = size * 8 * 1.5
windowHeightToPieceSize : Int -> Int
windowHeightToPieceSize height =
    (height * 2) // 24
    
-- width = (size * 7) * 1.1
windowWidthToPieceSize : Int -> Int
windowWidthToPieceSize width =
    (width * 10) // 77

windowSizeToPieceSize : Window.Size -> Int
windowSizeToPieceSize size =
    min (windowHeightToPieceSize size.height)
        (windowWidthToPieceSize size.width)

coloredPieceToString : ColoredPiece -> String
coloredPieceToString piece =
    (String.fromChar <| pieceToChar (Just piece))

printNodeName : Maybe Node -> String
printNodeName node =
    case node of
        Nothing ->
            "none"
        Just {name, piece} ->
            name ++ (case piece of
                         Nothing ->
                             ""
                         Just p ->
                             " " ++ (coloredPieceToString p)
                    )

printGameState : GameState -> String
printGameState gs =
    let {player, mode, isFirstMove, actor, subject} = gs
    in
        "player: " ++ (toString player) ++
        ", mode: " ++ (toString mode) ++
        ", firstMove: " ++ (toString isFirstMove) ++
        ", actor: " ++ (printNodeName actor) ++
        ", subject: " ++ (printNodeName subject)

printMessage : Message -> String
printMessage message =
    case message of
        NewRsp {gameid, playerid, name} ->
            "NewRsp, gameid: " ++ gameid ++ ", playerid: " ++ playerid ++
                ", name: " ++ name
        JoinRsp {playerid, names, gameState} ->
            "JoinRsp, playerid: " ++ playerid ++
                ", names: (" ++ names.white ++ ", " ++ names.black ++
                "), " ++ (printGameState gameState)
        UpdateRsp {gameid, gameState} ->
            "UpdateRsp, gameid: " ++ (printGameState gameState)
        _ ->
            encodeMessage message

calculateSelections : Model -> GameState -> List NodeSelection
calculateSelections model gs =
    let moves = gs.analysis.moves
    in
        case gs.mode of
            SetupMode ->
                case gs.subject of
                    Nothing ->
                        []
                    Just node ->
                        [ (placementSelectionColor, node.name) ]
            ChooseActorMode ->
                List.map (\actor -> (actorSelectionColor, actor))
                         <| Dict.keys moves
            ChooseSubjectMode ->
                case gs.actor of
                    Nothing ->
                        []
                    Just {name} ->
                        case Dict.get name moves of
                            Nothing ->
                                []
                            Just actorMoves ->
                                (actorSelectionColor, name) ::
                                (List.map
                                     (\{subject} ->
                                         (subjectSelectionColor, subject.name)
                                     )
                                     actorMoves
                                )
            ChooseTargetMode ->
                case gs.actor of
                    Nothing ->
                        []
                    Just actor ->
                        case Dict.get actor.name moves of
                            Nothing ->
                                []
                            Just actorMoves ->
                                case gs.subject of
                                    Nothing ->
                                        []
                                    Just {name} ->
                                        (actorSelectionColor, actor.name) ::
                                        (subjectSelectionColor, name) ::
                                        (List.map
                                             (\{target} ->
                                                  (targetSelectionColor, target.name)
                                             )
                                             (List.filter
                                                  (\move -> name == move.subject.name)
                                                  actorMoves
                                             )
                                        )
            _ ->
                []

serverMessage : ServerInterface Msg -> Message -> Model -> (Model, Cmd Msg)
serverMessage si message model =
    let mod = { model | server = si }
        ignore = log "message" <| printMessage message
    in
        case message of
            NewRsp { gameid, playerid } ->
                ( { mod
                      | gameid = gameid
                      , newGameid = gameid
                      , playerid = playerid
                      , nodeSelections = calculateSelections mod mod.gs
                  }
                , if model.isRemote then
                      Cmd.none
                  else
                      send mod.server
                          <| JoinReq { gameid = gameid
                                     , name = "Black"
                                     }
                )
            _ ->
                case message of
                    JoinRsp { playerid, names, gameState } ->
                        let m3 = if playerid == "" then
                                     mod
                                 else if model.isRemote then
                                          { mod | playerid = playerid }
                                      else
                                          { mod | otherPlayerid = playerid }
                        in
                            ( { m3
                                  | gs = gameState
                                  , names = names
                                  , nodeSelections =
                                      calculateSelections m3 gameState
                                  , chatSettings = makeChatSettings m3
                              }
                            , Cmd.none
                            )
                    UpdateRsp { gameState } ->
                        let gs = if model.isRemote then
                                     Board.addAnalysis gameState
                                 else
                                     gameState
                        in
                            ( { mod
                                  | gs = gs
                                  , nodeSelections = calculateSelections mod gs
                                  , you = if mod.isRemote then
                                              mod.you
                                          else
                                              gs.player
                              }
                            , Cmd.none
                            )
                    ChatRsp {gameid, player, text} ->
                        chatRsp mod player text
                    GamesRsp games ->
                        (mod, Cmd.none)
                    ErrorRsp { request, text } ->
                        ( { mod | message = Just text }
                        , Cmd.none
                        )
                    _ ->
                        (mod, Cmd.none)

chatRsp : Model -> Player -> String -> (Model, Cmd Msg)
chatRsp model player line =
    case model.chatSettings of
        Nothing ->
            (model, Cmd.none)
        Just settings ->
            let name = if player == model.you then
                           "You"
                       else if player == WhitePlayer then
                           model.names.white
                       else
                           model.names.black
                message = name ++ ": " ++ line
                (settings2, cmd) = ElmChat.addChat settings message
            in
                ( { model | chatSettings = Just settings2 }
                , cmd
                )

makeChatSettings : Model -> Maybe ChatSettings
makeChatSettings model =
    if not model.isRemote then
        Nothing
    else
        Just <| ElmChat.makeSettings "chatid" 14 True ChatUpdate

br : Html Msg
br =
    Html.br [][]

nbsp : String
nbsp =
    String.fromChar <| Char.fromCode 160

findSelection : String -> Model -> Maybe NodeSelection
findSelection name model =
    LE.find (\(_, nn) -> name == nn) model.nodeSelections

-- TODO
nodeMsg : Model -> NodeMsg
nodeMsg model board node =
    let piece = node.piece
        name = node.name
        gs = model.gs
    in
        case gs.mode of
            JoinMode ->
                Nothing
            GameOverMode ->
                Nothing
            SetupMode ->
                let which = case gs.player of
                                WhitePlayer -> TopList
                                BlackPlayer -> BottomList
                in
                    if board == gs.board then
                        if node.piece == Nothing && gs.subject /= Nothing then
                            Just <| NodeClick EmptyBoardClick which node
                        else
                            Nothing
                    else if piece /= Nothing then
                        if (gs.player == WhitePlayer && board == gs.topList) ||
                            (gs.player == BlackPlayer && board == gs.bottomList)
                        then
                            Just <| NodeClick SetupBoardClick which node
                        else
                            Nothing
                    else
                        Nothing
            ChooseActorMode ->
                case piece of
                    Nothing ->
                        Nothing
                    Just (_, piece) ->
                        case piece of
                            CenterHolePiece ->
                                if model.gs.isFirstMove then
                                    Nothing
                                else
                                    --Just otherPlayerClick
                                    Nothing
                            _ ->
                                case findSelection name model of
                                    Nothing ->
                                        Nothing
                                    _ ->
                                        Just
                                        <| NodeClick ChooseActorClick MainBoard node
            ChooseSubjectMode ->
                case piece of
                    Nothing ->
                        Nothing
                    Just _ ->
                        case findSelection name model of
                            Nothing ->
                                Nothing
                            Just (color, _) ->
                                if color == actorSelectionColor then
                                    Just <| NodeClick ChooseActorClick MainBoard node
                                else if color == subjectSelectionColor then
                                    Just <| NodeClick ChooseSubjectClick MainBoard node
                                else
                                    Nothing
            ChooseTargetMode ->
                case findSelection name model of
                    Nothing ->
                        Nothing
                    Just (color, _) ->
                        if color == actorSelectionColor then
                            Just <| NodeClick ChooseActorClick MainBoard node
                        else if color == subjectSelectionColor then
                            Just <| NodeClick ChooseSubjectClick MainBoard node
                        else
                            Just <| NodeClick ChooseTargetClick MainBoard node

playButton : Html Msg
playButton =
    p []
        [ button [ onClick <| SetPage GamePage
                 , style [("font-size", "150%")]
                 ]
              [ text "Play" ]
        ]

otherPlayerClick : Msg
otherPlayerClick =
    NodeClick OtherPlayerClick MainBoard centerHoleNode

endTurnButton : Model -> Html Msg
endTurnButton model =
    let gs = model.gs
        analysis = gs.analysis
        playMode = isPlayMode gs.mode
        hasMoves = not analysis.noMoves
        isKo = analysis.isKo
        otherKo = if isKo then
                      False
                  else
                      playMode &&
                      (not gs.isFirstMove || hasMoves) &&
                      gs.analysis.otherNoNonKoMoves
    in
        button [ onClick <| otherPlayerClick
               , pageLinksStyle
               -- Will eventually be disabled during Ko
               , disabled
                     <| (not playMode) ||
                        isKo ||
                        (gs.isFirstMove && hasMoves) ||
                        otherKo
               , title <| if isKo then
                              "The board is in a position it has been in before. You may not end your turn now."
                          else if otherKo then
                              "The other player has no sequence of moves that don't end in Ko. You may not end your turn now."
                          else
                              ""
               ]
        [ text <| if isKo then
                      "Ko"
                  else if otherKo then
                      "Other Ko"
                  else
                      "End Turn"
        ]

undoButton : Model -> Html Msg
undoButton model =
    button [ disabled <| model.gs.isFirstMove
           , pageLinksStyle
           , title "Click to undo the last move."
           , onClick Undo
           ]
        [ text "Undo" ]

iframe : Model -> String -> Html Msg
iframe model url =
    let s = case model.renderInfo of
                Nothing ->
                    []
                Just ri ->
                    let width = 7 * ri.cellSize
                    in
                        [ ("width", (toString width) ++ "px")
                        , ("height", "40em")
                        ]
    in
        Html.iframe [ style s
                    , src url
                    ]
            []

renderIframePage : Model -> String -> Html Msg
renderIframePage model url =
    div []
        [ playButton
        , iframe model url
        , playButton
        ]

renderRulesPage : Model -> Html Msg
renderRulesPage model =
    renderIframePage model "docs/rules.html"

renderHelpPage : Model -> Html Msg
renderHelpPage model =
    renderIframePage model "docs/help.html"

pages : List (Page, String)
pages =
    [ ( HelpPage, "Help" )
    --, ( PublicPage, "Public")
    , ( RulesPage, "Rules" )
    ]

pageLink : Page -> (Page, String) -> Html Msg
pageLink currentPage (page, label) =
    span [ pageLinksStyle ]
        [ text " "
        , if currentPage == page then
              span [ style [("font-weight", "bold")] ] [ text label ]
          else
              a [ href "#", onClick <| SetPage page ]
                  [ text label ]
        ]

pageLinksFontSize : String
pageLinksFontSize =
    "110%"

pageLinksStyle : Attribute Msg
pageLinksStyle =
    style [("font-size", pageLinksFontSize)]

pageLinks : Page -> Model -> Html Msg
pageLinks currentPage model =
    span []
        <| List.concat
            [ [ undoButton model ]
            , List.map (pageLink currentPage) pages
            , [ span [ pageLinksStyle ] [ text " " ]
              , endTurnButton model
              ]
            ]

view : Model -> Html Msg
view model =
    let gs = model.gs
        emptyBoard = Board.isEmptyBoard gs.board
    in
        div [ align "center"
            --deprecated, so sue me
            ]
        [ h2 [] [ text "Archmage" ]
        , p [] [ pageLinks model.page model ]
        , p []
            [ case model.message of
                  Nothing ->
                      text nbsp
                  Just m ->
                      text m
            ]
        , case model.page of
              GamePage ->
                  renderGamePage model
              PublicPage ->
                  text ""
              RulesPage ->
                  renderRulesPage model
              HelpPage ->
                  renderHelpPage model
        , p []
            [ input [ type_ "text"
                    , onInput <| if not emptyBoard then
                                     (\_ -> Noop)
                                 else
                                     SetRestoreState
                    , size 60
                    , if emptyBoard then
                          title "Enter a saved game to restore it."
                      else
                          let gs2 = if gs.mode == SetupMode then
                                        gs
                                    else
                                        { gs
                                        | actor = Nothing
                                        , subject = Nothing
                                        }
                          in
                              value <| encodeGameState gs2
                    ]
                  []
            , text " "
            , button [ onClick RestoreGame
                     , disabled <| not emptyBoard
                     ]
                  [ text "Restore" ]
            ]
        , footer
        ]

renderGamePage : Model -> Html Msg
renderGamePage model =
    let renderInfo = case model.renderInfo of
                         Nothing ->
                             Board.renderInfo <| log "defaultRenderInfo" pieceSize
                         Just ri ->
                             ri
        cellSize = renderInfo.cellSize
        locations = renderInfo.locations
        gs = model.gs
        listCellSize = case gs.mode of
                           JoinMode -> renderInfo.setupCellSize
                           SetupMode -> renderInfo.setupCellSize
                           _ -> renderInfo.captureCellSize
        (setupLocations, isPlay)
            = case gs.mode of
                  JoinMode -> (renderInfo.setupLineLocations, False)
                  SetupMode -> (renderInfo.setupLineLocations, False)
                  _ -> (renderInfo.captureLineLocations, True)
        sels = model.nodeSelections
        (topsel, boardsel, botsel) =
            case gs.mode of
                SetupMode ->
                    case gs.player of
                        WhitePlayer ->
                            (sels, [], [])
                        BlackPlayer ->
                            ([], [], sels)
                _ ->
                    ([], sels, [])
        isJoin = gs.mode == JoinMode
        tl = if isJoin && model.you == BlackPlayer then
                 Board.emptySetupBoard
             else
                 gs.topList
        b  = gs.board
        bl = if isJoin && model.you == WhitePlayer then
                 Board.emptySetupBoard
             else
                 gs.bottomList
        isRemote = model.isRemote
        newIsRemote = model.newIsRemote
        public = model.isPublic
        newGameid = model.newGameid
        joinMode = gs.mode == JoinMode
        modNodeMsg = if (isRemote && model.you /= gs.player) then
                         (\board node -> Nothing)
                     else
                         nodeMsg model
    in
        div []
            [ Board.render
                tl False setupLocations listCellSize topsel modNodeMsg
            , br
            , Board.render
                b True locations cellSize boardsel modNodeMsg
            , if isPlay then
                  text ""
              else
                  span []
                      [ br
                      , Board.render
                          bl False setupLocations listCellSize botsel modNodeMsg
                      ]
            , renderChat model
            , p [] [ pageLinks model.page model ]
            , p []
                [ checkbox "remote" newIsRemote False SetIsRemote
                , text " "
                , checkbox "public" public (not newIsRemote) SetIsPublic
                , text " "
                , button [ onClick NewGame ]
                      [ text "New Game" ]
                , if newIsRemote || (isRemote && joinMode)
                  then
                      span []
                          [ br
                          , text "Game ID: "
                          , input [ type_ "text"
                                  , onInput SetGameid
                                  , size 16
                                  , disabled joinMode
                                  , value <| if newGameid == dummyGameid then
                                                 ""
                                             else
                                                 newGameid
                                  ]
                              [ ]
                          , button [ onClick JoinGame
                                   , disabled <| gs.mode == JoinMode
                                   ]
                              [ text "Join Game" ]
                          ]
                  else
                      text ""
                ]
            ]

renderChat : Model -> Html Msg
renderChat model =
    case model.chatSettings of
        Nothing ->
            text ""
        Just settings ->
            div []
                [ ElmChat.chat settings
                , ElmChat.inputBox 30 "Send" ChatSend settings
                ]

checkbox : String -> Bool -> Bool -> (Bool -> msg) -> Html msg
checkbox text_ isChecked isDisabled checker =
    span []
        [ input [ type_ "checkbox"
                , onCheck checker
                , checked isChecked
                , disabled isDisabled
                ]
              []
        , text text_
        ]

footer : Html Msg
footer =
    div []
        [ p []
            [ a [ href "https://gibgoygames.com/"
                , target "_blank"
                ]
                  [ text "Gib Goy Games" ]
            , text " "
            , a [ href "https://github.com/billstclair/archmage"
                , target "_blank"
                ]
                  [ text "GitHub" ]
            ]
        , p [] [ text "Invented by Chris St. Clair"
               , br
               , text "Coded by Bill St. Clair"
               , br
               , text "Made with "
               , a [ href "http://elm-lang.org/"
                   , target "_blank"
                   ]
                   [ text "Elm" ]
               ]
        ]
