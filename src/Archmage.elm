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
    exposing ( GameState, Piece(..), Color(..), Player(..)
             , ColoredPiece, Board, Node
             , NodeSelection, RenderInfo
             , Page(..), Msg(..), Mode(..), ClickKind(..), WhichBoard(..)
             , NodeMsg, MovesDict
             , otherColor, playerColor, otherPlayer
             , setBoardPiece, pieceToAbbreviation
             )
import Archmage.Pieces exposing ( drawPiece )
import Archmage.Board as Board exposing ( initialGameState, getNode, printMove )

import Html exposing ( Html, Attribute , div, h2, text, img, p, a, button, span )
import Html.Attributes exposing ( align, src, href, target, style )
import Html.Events exposing ( onClick )
import Svg exposing ( Svg, svg, g, rect )
import Svg.Attributes exposing ( x, y, width, height, stroke, strokeWidth, fillOpacity )
import Char
import Dict exposing ( Dict )
import List.Extra as LE
import Task
import Debug exposing ( log )

type alias Model =
    { page : Page
    , moves : MovesDict
    , nodeSelections : List NodeSelection
    , renderInfo : RenderInfo
    , message : Maybe String
    , gs : GameState
    }

main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\m -> Sub.none)
        }

placementSelectionColor = "black"
otherPlayerSelectionColor = "orange"
actorSelectionColor = "green"
subjectSelectionColor = "blue"
targetSelectionColor = "red"

messages : List (Mode, String)
messages =
    [ (SetupMode, "select and place a piece")
    , (ChooseActorMode, "click a " ++ actorSelectionColor
           ++ "-highighted actor")
    , (ChooseSubjectMode, "click a " ++ subjectSelectionColor
           ++ "-highlighted subject or the actor")
    , (ChooseTargetMode, "click a " ++ targetSelectionColor
           ++ "-highlighted target, the subject, or the actor")
    , (GameOverMode, "Game Over!")
    ]

setMessage : Model -> Model
setMessage model =
    let gs = model.gs
    in
        case Types.get gs.mode messages of
            Nothing ->
                { model | message = Nothing }
            Just message ->
                let c = case gs.player of
                            WhitePlayer -> "White, "
                            BlackPlayer -> "Black, "
                    msg = case gs.mode of
                              GameOverMode ->
                                  message
                              _ ->
                                  let suffix = if gs.isFirstMove then
                                                   "."
                                               else
                                                   case gs.mode of
                                                       ChooseActorMode ->
                                                           " or the black center square."
                                                       _ ->
                                                           "."
                                  in
                                      c ++ message ++ suffix
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

-- Set true to place all the pieces at startup.
-- Used to speed debugging of the move code.
doPlaceAll : Bool
doPlaceAll = False --True

init : ( Model, Cmd Msg )
init =
    let mod = { page = GamePage
              , moves = Dict.empty
              , nodeSelections = []
              , renderInfo = Board.renderInfo pieceSize
              , message = Nothing
              , gs = initialGameState doPlaceAll
              }
        model = if not doPlaceAll then
                    { mod
                        | nodeSelections = initialPlacementSelections mod.gs.player mod
                    }
                else
                    findValidMoves mod
    in
        ( model, Cmd.none )

whichBoard : WhichBoard -> Model -> Board
whichBoard which model =
    case which of
        TopList -> model.gs.topList
        BottomList -> model.gs.bottomList
        MainBoard -> model.gs.board

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case log "" msg of
        NewGame ->
            init
        SetPage page ->
            ( { model | page = page }
            , Cmd.none
            )
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
                                }
                      in
                          findValidMoves
                          { model
                              | gs = gs2
                          }
                    , Cmd.none
                    )
                ChooseActorClick ->
                    let subjectSelections =
                            case Dict.get node.name model.moves of
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
                            case Dict.get actorName model.moves of
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
                    chooseTargetClick node model
        _ ->
            ( model, Cmd.none )

chooseTargetClick : Node -> Model -> (Model, Cmd Msg)
chooseTargetClick target model =
    case model.gs.subject of
        Nothing ->
            (model, Cmd.none)   --can't happen
        Just subject ->
            let b = case model.gs.actor of
                        Nothing ->
                            model.gs.board --can't happen
                        Just actor ->
                            case actor.piece of
                                Nothing ->
                                    model.gs.board --can't happen
                                Just (color, piece) ->
                                    setBoardPiece
                                        actor.name
                                        (Just (otherColor color, piece))
                                        model.gs.board
                b2 = setBoardPiece subject.name Nothing b
                gs = model.gs
                mod = case target.piece of
                          Just _ ->
                              addPieceToCaptureBoard subject.piece
                                  { model |
                                        gs = { gs | board = b2 }
                                  }
                          Nothing ->
                              { model |
                                    gs = { gs | board =
                                                  setBoardPiece
                                                      target.name
                                                      subject.piece
                                                      b2
                                         }
                              }
                gs2 = mod.gs
            in
                ( findValidMoves
                      { mod
                          | gs = { gs2
                                     | mode = ChooseActorMode
                                     , isFirstMove = False
                                     , subject = Nothing
                                 }
                      }
                , Cmd.none
                )

addPieceToCaptureBoard : Maybe ColoredPiece -> Model -> Model
addPieceToCaptureBoard coloredPiece model =
    case coloredPiece of
        Nothing ->
            model
        Just (color, piece) ->
            let board = case color of
                            Black -> model.gs.topList
                            White -> model.gs.bottomList
                letter = pieceToAbbreviation piece
                maybeName = letter ++ "1"
                name = case getNode maybeName board of
                           Nothing ->
                               maybeName --can't happen
                           Just node ->
                               if node.piece == Nothing then
                                   maybeName
                               else
                                   letter ++ "2"
                newBoard = setBoardPiece name coloredPiece board
                gs = model.gs
            in
                case color of
                    Black ->
                        { model
                            | gs = { gs | topList = newBoard }
                        }
                    White ->
                        { model
                            | gs = { gs | bottomList = newBoard }
                        }

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
                mod3 = if gs2.mode == SetupMode then
                           mod2
                       else
                           findValidMoves
                           { mod2
                               | gs = { gs3
                                          | topList = Board.initialCaptureBoard
                                          , bottomList = Board.initialCaptureBoard
                                      }
                           }
            in
                ( mod3 , Cmd.none )


findValidMoves : Model -> Model
findValidMoves model =
    let gs = model.gs
        moves = Board.validMoves (playerColor gs.player) gs.board
    in
        case highlightActors moves model of
            Just m ->
                { m | moves = moves }
            Nothing ->
                let player = otherPlayer gs.player
                    otherMoves = Board.validMoves (playerColor player) gs.board
                    gs2 = { gs
                              | isFirstMove = True
                              , player = player
                          }
                    mod = { model
                              | gs = gs2
                              , moves = otherMoves
                          }
                in
                    case highlightActors otherMoves mod of
                        Nothing ->
                            { mod
                                | gs = { gs
                                           | mode = GameOverMode
                                       }
                                , nodeSelections = []
                            }
                        Just m2 ->
                            m2

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
                                   [(otherPlayerSelectionColor, "D3")]
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
                                    Just
                                    <| NodeClick OtherPlayerClick MainBoard node
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

pageLinks : Page -> Html Msg
pageLinks currentPage =
    span []
        <| List.map (pageLink currentPage) pages

view : Model -> Html Msg
view model =
    let mod = setMessage model
    in
        div [ align "center"
            --deprecated, so sue me
            ]
        [ h2 [] [ text "Archmage" ]
        , p [] [ pageLinks model.page ]
        , p []
            [ case mod.message of
                  Nothing ->
                      text nbsp
                  Just m ->
                      text m
            ]
        , case model.page of
              GamePage ->
                  renderGamePage mod
              PublicPage ->
                  text ""
              RulesPage ->
                  renderRulesPage mod
              HelpPage ->
                  renderHelpPage mod
        , p [] [ pageLinks model.page ]
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
    in
        div []
            [ Board.render
                  gs.topList setupLocations listCellSize topsel modNodeMsg
            , br
            , Board.render
                gs.board locations cellSize boardsel modNodeMsg
            , br
            , Board.render
                gs.bottomList setupLocations listCellSize botsel modNodeMsg
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
