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
             , ServerInterface, Message(..)
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
import Archmage.Server.Interface exposing ( makeProxyServer, makeServer, send )

import Html exposing ( Html, Attribute , div, h2, text, img, p, a, button, span
                     , input
                     )
import Html.Attributes exposing ( align, src, href, target, style, disabled, title
                                , type_, size, value
                                )
import Html.Events exposing ( onClick, onInput )
import Svg exposing ( Svg, svg, g, rect )
import Svg.Attributes exposing ( x, y, width, height, stroke, strokeWidth, fillOpacity )
import Char
import Dict exposing ( Dict )
import List.Extra as LE
import Task
import Debug exposing ( log )

type alias Model =
    { page : Page
    , nodeSelections : List NodeSelection
    , renderInfo : RenderInfo
    , message : Maybe String
    , restoreState : String
    , gs : GameState
    , isRemote : Bool
    , server : ServerInterface Msg
    , gameid : String
    , names : PlayerNames
    }

main =
    Html.program
        { init = init Nothing
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

subscriptions: Model -> Sub Msg
subscriptions model =
    Sub.none

{-

A few words about end of game.

-}

placementSelectionColor = "black"
otherPlayerSelectionColor = "orange"
actorSelectionColor = "green"
subjectSelectionColor = "blue"
targetSelectionColor = "red"

messages : List (Mode, String)
messages =
    [ (SetupMode, "select and place a piece.")
    , (ChooseActorMode, "click a " ++ actorSelectionColor
           ++ "-highighted actor.")
    , (ChooseSubjectMode, "click a " ++ subjectSelectionColor
           ++ "-highlighted subject or the actor.")
    , (ChooseTargetMode, "click a " ++ targetSelectionColor
           ++ "-highlighted target, the subject, or the actor.")
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
                let c = case gs.player of
                            WhitePlayer -> "White, "
                            BlackPlayer -> "Black, "
                    analysis = gs.analysis
                    msg2 = if analysis.noNonKoMoves then
                               if analysis.otherNoNonKoMoves then
                                   "Every sequence of moves ends in Ko for both players. Undo."
                               else
                                   let suffix = if gs.isFirstMove then
                                                    "Click \"End Turn\"."
                                                else
                                                    "Undo or click \"End Turn\"."
                                   in
                                       "Every sequence of moves end in Ko. " ++ suffix
                           else if gs.mode == ChooseActorMode
                                   && Dict.isEmpty analysis.moves
                           then
                               if analysis.otherNoNonKoMoves then
                                   "the other player has no non-Ko moves. Undo."
                               else if gs.isFirstMove then
                                   "no moves are possible. Click \"End Turn\"."
                               else if analysis.isKo then
                                   "you're in Ko and no moves are possible. Undo."
                               else
                                   "no moves are possible. Undo or click \"End Turn\"."
                           else
                               message
                    msg = if gs.mode == GameOverMode then
                              if analysis.noNonKoMoves && analysis.otherNoNonKoMoves then
                                  "Game Over! Every sequence of moves ends in Ko for both players."
                              else
                                  message
                          else
                              c ++ msg2
                in
                    { model | message = Just <| msg }

initialPlacementSelections : Player -> Model -> List NodeSelection
initialPlacementSelections player model =
    let gs = model.gs
        board = case player of
                    WhitePlayer -> gs.topList
                    BlackPlayer -> gs.bottomList
        nodes = Dict.values board.nodes
                |> List.sortBy .column
    in
        case LE.find (\node -> node.piece /= Nothing) nodes
        of
            Nothing -> []
            Just node -> [ (placementSelectionColor, node.name) ]

pieceSize : Int
pieceSize =
    100

init : Maybe Model -> ( Model, Cmd Msg )
init maybeModel =
    let (model, restoreState)
        = case maybeModel of
              Just mod ->
                  (mod, Just mod.gs)
              Nothing ->
                  let mod = { page = GamePage
                            , nodeSelections = []
                            , renderInfo = Board.renderInfo pieceSize
                            , message = Nothing
                            , restoreState = ""
                            , gs = initialGameState False
                            , isRemote = False
                            , server = makeProxyServer ServerMessage
                            , gameid = ""
                            , names = initialPlayerNames
                            }
                  in
                      ( { mod
                            | nodeSelections
                                = initialPlacementSelections mod.gs.player mod
                        }
                      , Nothing
                      )
    in
        ( model
        , send model.server
            <| NewReq { name = model.names.white
                      , isPublic = False
                      , restoreState = Nothing
                      }
        )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let (mod, cmd) = updateInternal msg { model | message = Nothing }
        mod2 = if mod.message == Nothing then
                   setMessage mod
               else
                   mod
    in
        (mod2, cmd)
        
updateInternal : Msg -> Model -> ( Model, Cmd Msg )
updateInternal msg model =
    case msg of
        NewGame ->
            init Nothing
        ServerMessage si message ->
            serverMessage si message model
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
                <| UndoReq { gameid = model.gameid }
            )
        NodeClick kind which node ->
            case kind of
                SetupBoardClick ->
                    ( model
                    , send model.server
                        <| SelectPlacementReq { gameid = model.gameid
                                              , node = node.name
                                              }
                    )
                EmptyBoardClick ->
                    ( model
                    , send model.server
                        <| PlaceReq { gameid = model.gameid
                                    , node = node.name
                                    }
                    )
                OtherPlayerClick ->
                    ( model
                    , send model.server
                        <| EndTurnReq { gameid = model.gameid }
                    )
                ChooseActorClick ->
                    ( model
                    , send model.server
                        <| SelectActorReq { gameid = model.gameid
                                          , node = node.name
                                          }
                    )
                ChooseSubjectClick ->
                    ( model
                    , send model.server
                        <| SelectSubjectReq { gameid = model.gameid
                                            , node = node.name
                                            }
                    )
                ChooseTargetClick ->
                    ( model
                    , send model.server
                        <| MoveReq { gameid = model.gameid
                                   , node = node.name
                                   }
                    )
        _ ->
            ( model, Cmd.none )

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
        JoinRsp {gameid, names, gameState} ->
            "JoinRsp, gameid: " ++ gameid ++
                ", names: (" ++ names.white ++ ", " ++ names.black ++
                "), " ++ (printGameState gameState)
        UpdateRsp {gameid, gameState} ->
            "UpdateRsp, gameid: " ++ (printGameState gameState)
        _ ->
            encodeMessage message

calculateSelections : GameState -> List NodeSelection
calculateSelections gs =
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
            NewRsp { gameid } ->
                ( { mod
                      | gameid = gameid
                  }
                -- This will be Cmd.none when we're in remote mode
                , send mod.server
                    <| JoinReq { gameid = gameid
                               , name = "Black"
                               }
                )
            _ ->
                let m2 = case message of
                             JoinRsp { gameid, names, gameState } ->
                                 { mod
                                     | gameid = gameid
                                     , gs = gameState
                                     , names = names
                                     , nodeSelections = calculateSelections gameState
                                 }
                             UpdateRsp { gameState } ->
                                 let gs = if model.isRemote then
                                              Board.addAnalysis gameState
                                          else
                                              gameState
                                 in
                                     { mod
                                         | gs = gs
                                         , nodeSelections = calculateSelections gs
                                     }
                             GamesRsp games ->
                                 mod
                             ErrorRsp { request, text } ->
                                 { mod | message = Just text }
                             ChatRsp { gameid, player, text } ->
                                 -- TODO
                                 mod
                             _ ->
                                 mod
                in
                    (m2, Cmd.none)

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
                                    Just otherPlayerClick
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
               -- Will eventually be disabled during Ko
               , disabled
                     <| (not playMode) || isKo ||
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
           , title "Click to undo the last move."
           , onClick Undo
           ]
        [ text "Undo" ]

iframe : String -> Html Msg
iframe url =
    Html.iframe [ style [ ("width", "40em")
                        , ("height", "40em")
                        ]
                , src url
                ]
        []

renderIframePage : Model -> String -> Html Msg
renderIframePage model url =
    div []
        [ playButton
        , iframe url
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
    span []
        [ text " "
        , if currentPage == page then
              span [ style [("font-weight", "bold")] ] [ text label ]
          else
              a [ href "#", onClick <| SetPage page ]
                  [ text label ]
        ]

pageLinks : Page -> Model -> Html Msg
pageLinks currentPage model =
    span []
        <| List.concat
            [ [ endTurnButton model ]
            , List.map (pageLink currentPage) pages
            , [ text " "
              , undoButton model
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
        , p [] [ pageLinks model.page model ]
        , footer
        ]

renderGamePage : Model -> Html Msg
renderGamePage model =
    let renderInfo = model.renderInfo
        cellSize = renderInfo.cellSize
        locations = renderInfo.locations
        gs = model.gs
        listCellSize = if gs.mode == SetupMode then
                           renderInfo.setupCellSize
                       else
                           renderInfo.captureCellSize
        setupLocations = case gs.mode of
                             SetupMode -> renderInfo.setupLineLocations
                             _ -> renderInfo.captureLineLocations
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
        modNodeMsg = nodeMsg model
        bsb = (\b ->
                   b --stringToBoard <| boardToString b
              )
        tl = bsb gs.topList
        b  = bsb gs.board
        bl = bsb gs.bottomList
    in
        div []
            [ Board.render
                tl False setupLocations listCellSize topsel modNodeMsg
            , br
            , Board.render
                b True locations cellSize boardsel modNodeMsg
            , br
            , Board.render
                bl False setupLocations listCellSize botsel modNodeMsg
            , newGameButton
            ]

newGameButton : Html Msg
newGameButton =
    p []
        [ button [ onClick NewGame ]
              [ text "New Game" ]
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
