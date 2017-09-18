----------------------------------------------------------------------
--
-- Archmage.elm
-- Chris St. Clair's Archmage board game.
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

module Archmage exposing (..)

import Archmage.Types as Types
    exposing ( GameState, TheGameState(..), Piece(..), Color(..), Player(..)
             , ColoredPiece, Board, Node
             , NodeSelection, RenderInfo
             , Page(..), Msg(..), Mode(..), ClickKind(..), WhichBoard(..)
             , NodeMsg, MovesDict
             , ServerInterface, Message(..)
             , otherColor, playerColor, otherPlayer
             , setBoardPiece
             )
import Archmage.Pieces exposing ( drawPiece )
import Archmage.Board as Board exposing ( initialGameState, getNode, printMove
                                        , makeMove, isPlayMode
                                        , boardToString, stringToBoard
                                        , centerHoleName, centerHoleNode
                                        )
import Archmage.Server.EncodeDecode exposing ( encodeGameState, restoreGame )
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
    , server : ServerInterface Msg
    }

main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\m -> Sub.none)
        }

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
                    msg2 = if gs.mode == ChooseActorMode
                             && Dict.isEmpty gs.analysis.moves
                           then
                               if gs.analysis.isKo then
                                   "you're in Ko and no moves are possible. Undo."
                               else if gs.isFirstMove then
                                   "no moves are possible. Click \"End Turn\"."
                               else
                                   "no moves are possible. Undo or click \"End Turn\"."
                           else
                               message
                    msg = if gs.mode == GameOverMode then
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

init : ( Model, Cmd Msg )
init =
    let mod = { page = GamePage
              , nodeSelections = []
              , renderInfo = Board.renderInfo pieceSize
              , message = Nothing
              , restoreState = ""
              , gs = initialGameState False
              , server = makeProxyServer ServerMessage
              }
        model = { mod
                    | nodeSelections = initialPlacementSelections mod.gs.player mod
                }
    in
        ( model
        , send model.server
            <| NewReq { name ="White"
                      , isPublic = False
                      , restoreState = Nothing
                      }
        )

whichBoard : WhichBoard -> Model -> Board
whichBoard which model =
    case which of
        TopList -> model.gs.topList
        BottomList -> model.gs.bottomList
        MainBoard -> model.gs.board

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
    case log "" msg of
        NewGame ->
            init
        ServerMessage si message ->
            serverMessage si message model
        SetRestoreState text ->
            ( { model | restoreState = text }
            , Cmd.none
            )
        RestoreGame ->
            case restoreGame model.restoreState of
                Ok gs ->
                    let gs2 = { gs | mode = if isPlayMode gs.mode then
                                                ChooseActorMode
                                            else
                                                gs.mode
                                    , actor = Nothing
                                    , subject = Nothing
                              }
                        mod = { model | gs = gs2 }
                    in
                        ( if isPlayMode gs2.mode then
                              findValidMoves True mod
                          else
                              { mod |
                                    nodeSelections =
                                        initialPlacementSelections gs.player mod
                              }
                        , Cmd.none
                        )
                Err _ ->
                    ( { model | message = Just "Invalid game state." }
                    , Cmd.none
                    )
        SetPage page ->
            ( { model | page = page }
            , Cmd.none
            )
        Undo ->
            case model.gs.undoState of
                Just (TheGameState gs) ->
                    ( findValidMoves False { model | gs = Board.addAnalysis gs }
                    , Cmd.none
                    )
                _ ->
                    ( model, Cmd.none )
        NodeClick kind which node ->
            case kind of
                SetupBoardClick ->
                    ( { model
                          | nodeSelections = [ (placementSelectionColor, node.name) ]
                      }
                    , Cmd.none
                    )
                EmptyBoardClick ->
                    case model.gs.mode of
                        SetupMode ->
                            setupEmptyBoardClick which node model
                        _ ->
                            (model, Cmd.none)
                OtherPlayerClick ->
                    ( let gs = model.gs
                          gs2 = { gs
                                    | player = otherPlayer gs.player
                                    , isFirstMove = True
                                    , mode = ChooseActorMode
                                    , undoState = Nothing
                                    , history = Board.boardToString gs.board
                                                :: gs.history
                                }
                      in
                          findValidMoves
                              True { model | gs = Board.addAnalysis gs2 }
                    , Cmd.none
                    )
                ChooseActorClick ->
                    let subjectSelections =
                            case Dict.get node.name model.gs.analysis.moves of
                                Nothing ->
                                    []
                                Just moves ->
                                    List.map (\move ->
                                                  ( subjectSelectionColor
                                                  , move.subject.name
                                                  )
                                             )
                                        moves
                        gs = model.gs
                        gs2 = { gs
                                  | mode = ChooseSubjectMode
                                  , actor = Just node
                              }
                    in
                        ( { model
                              | gs = gs2
                              , nodeSelections =
                                (actorSelectionColor, node.name) :: subjectSelections
                          }
                        , Cmd.none
                        )
                UnchooseActorClick ->
                    let gs = model.gs
                    in
                        ( findValidMoves
                              False
                              { model
                                  | gs = { gs | mode = ChooseActorMode 
                                         , subject = Nothing
                                         }
                              }
                        , Cmd.none
                        )
                ChooseSubjectClick ->
                    let actorName = case model.gs.actor of
                                        Just actor ->
                                            actor.name
                                        Nothing ->
                                            "H0" --can't happen
                        targetSelections =
                            case Dict.get actorName model.gs.analysis.moves of
                                Nothing ->
                                    []
                                Just moves ->
                                    let targetMoves =
                                            List.filter
                                                (\move -> move.subject == node)
                                                moves
                                    in
                                        List.map (\move ->
                                                      ( targetSelectionColor
                                                      , move.target.name
                                                      )
                                                 )
                                            targetMoves
                        gs = model.gs
                        gs2 = { gs
                                  | mode = ChooseTargetMode
                                  , subject = Just node
                              }
                    in
                        ( { model
                              | gs = gs2
                              , nodeSelections =
                                List.append
                                    [ (actorSelectionColor, actorName)
                                    , (subjectSelectionColor, node.name)
                                    ]
                                    targetSelections
                          }
                        , Cmd.none
                        )
                UnchooseSubjectClick ->
                    case model.gs.actor of
                        Nothing ->
                            (model, Cmd.none) --can't happen
                        Just actor ->
                            let gs = model.gs
                                gs2 = { gs | mode = ChooseActorMode }
                            in
                                update (NodeClick ChooseActorClick MainBoard actor)
                                    { model | gs = gs2 }
                ChooseTargetClick ->
                    ( findValidMoves
                          False
                          { model
                              | gs = makeMove node.name model.gs
                          }
                    , Cmd.none
                    )
        _ ->
            ( model, Cmd.none )

-- ** CONTINUE HERE **
serverMessage : ServerInterface Msg -> Message -> Model -> (Model, Cmd Msg)
serverMessage si message model =
    let mod = { model | server = si }
        m2 = case message of
                 NewRsp { gameid, name } -> mod
                 JoinRsp { gameid, names, gameState } -> mod
                 UpdateRsp { gameid, gameState } -> mod
                 GamesRsp games -> mod
                 ErrorRsp { request, text } -> mod
                 ChatRsp { gameid, player, text } -> mod
                 _ -> mod
    in
        (m2, Cmd.none)

setupEmptyBoardClick : WhichBoard -> Node -> Model -> (Model, Cmd Msg)
setupEmptyBoardClick which node model =
    case model.nodeSelections of
        [] ->
            (model, Cmd.none)
        (_, sn) :: _ ->
            let board = whichBoard which model
                gs = model.gs
                gs2 = case which of
                          TopList ->
                              { gs
                                  | topList =
                                    setBoardPiece
                                    sn Nothing board
                              }
                          BottomList ->
                              { gs
                                  | bottomList =
                                    setBoardPiece
                                    sn Nothing board
                              }
                          _ ->
                              gs
                player = case which of
                             TopList -> BlackPlayer
                             BottomList -> WhitePlayer
                             _ -> gs2.player
                selections = initialPlacementSelections player model
                list = case which of
                           TopList -> model.gs.topList
                           _ -> model.gs.bottomList
                piece = case getNode sn list of
                            Nothing -> Nothing
                            Just n -> n.piece
                gs3 = { gs2
                          | board =
                               setBoardPiece node.name piece gs2.board
                           , player = player
                           , mode = if selections == [] then
                                        ChooseActorMode
                                    else
                                        SetupMode
                      }
                mod2 = { model
                           | nodeSelections = selections
                           , gs = gs3
                       }
                mod3 = if gs3.mode == SetupMode then
                           mod2
                       else
                           findValidMoves
                               True
                               { mod2
                                   | gs = Board.addAnalysis
                                          { gs3
                                              | topList = Board.initialCaptureBoard
                                              , bottomList = Board.initialCaptureBoard
                                              , history = [boardToString gs3.board]
                                          }
                               }
            in
                ( mod3 , Cmd.none )


checkForNonKo : Model -> (Model, Bool)
checkForNonKo model =
    let gs = model.gs
        analysis = gs.analysis
        firstMove = gs.isFirstMove
    in
        if not analysis.noNonKoMoves then
            (model, False)
        else if not firstMove || not analysis.otherNoNonKoMoves then
            let suffix = if firstMove then
                             "Click \"End Turn\"."
                         else
                             "Undo or click \"End Turn\"."
                msg = "Every sequence of moves ends in Ko. " ++ suffix
            in
                ( { model
                      | message = Just msg
                      , nodeSelections = []
                  }
                , True
                )
        else
            ( { model
                  | nodeSelections = []
                  , message = Just "Game Over! Every sequence of moves ends in Ko for both players."
              }
            , True
            )
            
findValidMoves : Bool -> Model -> Model
findValidMoves gameOverIfNone model =
    let gs = model.gs
        analysis = gs.analysis
        moves = analysis.moves
        mod = { model | nodeSelections = [] }
        (mod2, done) = if Dict.isEmpty moves then
                           (mod, False)
                       else
                           checkForNonKo mod
    in
        if done then
            mod2
        else
            case highlightActors moves mod2 of
                Just m ->
                    m
                Nothing ->
                    if not gameOverIfNone then
                        mod2
                    else
                        let player = otherPlayer gs.player
                            otherMoves =
                                Board.validMoves (playerColor player) gs.board
                        in
                            if Dict.isEmpty otherMoves then
                                { mod2 | gs = { gs | mode = GameOverMode } }
                            else
                                mod2

highlightActors : MovesDict -> Model -> Maybe Model
highlightActors moves model =
    case Dict.keys moves of
        [] ->
            Nothing
        keys ->
            Just { model
                     | nodeSelections =
                         List.concat
                             [ List.map (\key -> (actorSelectionColor, key)) keys
                             , if model.gs.isFirstMove then
                                   []
                               else
                                   [(otherPlayerSelectionColor, centerHoleName)]
                             ]
                 }

pieceSize : Int
pieceSize =
    100

pieceCount : Int
pieceCount =
    7

pieces : List Piece
pieces =
    [ HandPiece
    , CupPiece
    , SwordPiece
    , WandPiece
    , TowerPiece
    , MoonPiece
    , MagePiece
    ]

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
                        if node.piece == Nothing && model.nodeSelections /= [] then
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
                                    Just <| NodeClick UnchooseActorClick MainBoard node
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
                            Just <| NodeClick UnchooseActorClick MainBoard node
                        else if color == subjectSelectionColor then
                            Just <| NodeClick UnchooseSubjectClick MainBoard node
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
                          let gs2 = { gs
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
