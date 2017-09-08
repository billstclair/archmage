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
    exposing ( Piece(..), Color(..), Board, Node
             , NodeSelection, RenderInfo
             , Msg(..), Mode(..), ClickKind(..), WhichBoard(..)
             , NodeMsg, MovesDict
             , setBoardPiece
             )
import Archmage.Pieces exposing ( drawPiece )
import Archmage.Board as Board exposing ( getNode )

import Html exposing ( Html, Attribute , div, h2, text, img, p, a )
import Html.Attributes exposing ( align, src, href, target )
import Svg exposing ( Svg, svg, g, rect )
import Svg.Attributes exposing ( x, y, width, height, stroke, strokeWidth, fillOpacity )
import Char
import Dict exposing ( Dict )
import List.Extra as LE
import Task
import Debug exposing ( log )

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

type alias Model =
    { mode : Mode
    , isFirstMove : Bool
    , player : Player
    , moves : MovesDict
    , nodeSelections : List NodeSelection
    , actor : Maybe Node
    , subject : Maybe Node
    , board : Board
    , topList : Board
    , bottomList : Board
    , renderInfo : RenderInfo
    , message : Maybe String
    }

main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\m -> Sub.none)
        }

placementSelectionColor = "black"
actorSelectionColor = "green"
subjectSelectionColor = "blue"
targetSelectionColor = "red"

messages : List (Mode, String)
messages =
    [ (SetupMode, "select and place a piece")
    , (ChooseActorMode, "click a " ++ actorSelectionColor
           ++ "-highighted actor")
    , (ChooseSubjectMode, "click a " ++ subjectSelectionColor
           ++ "-highlighted subject")
    , (ChooseTargetMode, "click a " ++ targetSelectionColor
           ++ "-highlighted target, the subject")
    , (GameOverMode, "Game Over!")
    ]

setMessage : Model -> Model
setMessage model =
    case Types.get model.mode messages of
        Nothing ->
            { model | message = Nothing }
        Just message ->
            let c = case model.player of
                        WhitePlayer -> "White, "
                        BlackPlayer -> "Black, "
                msg = case model.mode of
                          GameOverMode ->
                              message
                          _ ->
                              let suffix = if model.isFirstMove then
                                               case model.mode of
                                                   ChooseSubjectMode ->
                                                       " or the actor."
                                                   ChooseTargetMode ->
                                                       ", or the actor."
                                                   _ ->
                                                       "."
                                           else
                                               case model.mode of
                                                   SetupMode ->
                                                       "."
                                                   ChooseActorMode ->
                                                       " or the black center square."
                                                   ChooseSubjectMode ->
                                                       ", the actor, or the black center square."
                                                   ChooseTargetMode ->
                                                       ", the actor, or the black center square."
                                                   _ ->
                                                       "."
                              in
                                  c ++ message ++ suffix
            in
                { model | message = Just <| msg }

initialPlacementSelections : Player -> Model -> List NodeSelection
initialPlacementSelections player model =
    let board = case player of
                    WhitePlayer -> model.topList
                    BlackPlayer -> model.bottomList
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
doPlaceAll = True

init : ( Model, Cmd Msg )
init =
    let mod = { mode = SetupMode
              , isFirstMove = True
              , player = WhitePlayer
              , moves = Dict.empty
              , nodeSelections = []
              , actor = Nothing
              , subject = Nothing
              , board = Board.initialBoard
              , topList = Board.whiteSetupBoard
              , bottomList = Board.blackSetupBoard
              , renderInfo = Board.renderInfo pieceSize
              , message = Nothing
              }
        model = if not doPlaceAll then
                    { mod
                        | nodeSelections = initialPlacementSelections mod.player mod
                    }
                else
                    findValidMoves
                    { mod
                        | board = Board.dummyBoard
                        , topList = Board.initialCaptureBoard
                        , bottomList = Board.initialCaptureBoard
                        , mode = ChooseActorMode
                    }
    in
        ( model, Cmd.none )

whichBoard : WhichBoard -> Model -> Board
whichBoard which model =
    case which of
        TopList -> model.topList
        BottomList -> model.bottomList
        MainBoard -> model.board

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case log "" msg of
        NodeClick kind which node ->
            case kind of
                SetupBoardClick ->
                    ( { model
                          | nodeSelections = [ (placementSelectionColor, node.name) ]
                      }
                    , Cmd.none
                    )
                EmptyBoardClick ->
                    case model.mode of
                        SetupMode ->
                            setupEmptyBoardClick which node model
                        _ ->
                            (model, Cmd.none)
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
                    in
                        ( { model
                              | mode = ChooseSubjectMode
                              , actor = Just node
                              , nodeSelections =
                                (actorSelectionColor, node.name) :: subjectSelections
                          }
                        , Cmd.none
                        )
                _ ->
                    (model, Cmd.none)
        _ ->
            ( model, Cmd.none )

setupEmptyBoardClick : WhichBoard -> Node -> Model -> (Model, Cmd Msg)
setupEmptyBoardClick which node model =
    case model.nodeSelections of
        [] ->
            (model, Cmd.none)
        (_, sn) :: _ ->
            let board = whichBoard which model
                mod = case which of
                          TopList ->
                              { model
                                  | topList =
                                    setBoardPiece
                                    sn Nothing board
                              }
                          BottomList ->
                              { model
                                  | bottomList =
                                    setBoardPiece
                                    sn Nothing board
                              }
                          _ ->
                              model
                player = case which of
                             TopList -> BlackPlayer
                             BottomList -> WhitePlayer
                             _ -> mod.player
                selections = initialPlacementSelections player model
                list = case which of
                           TopList -> model.topList
                           _ -> model.bottomList
                piece = case getNode sn list of
                            Nothing -> Nothing
                            Just n -> n.piece
                mod2 = { mod
                           | board =
                               setBoardPiece node.name piece mod.board
                           , nodeSelections = selections
                           , player = player
                           , mode = if selections == [] then
                                        -- Need to compute selections
                                        ChooseActorMode
                                    else
                                        SetupMode
                       }
                mod3 = if mod2.mode == SetupMode then
                           mod2
                       else
                           findValidMoves
                           { mod2
                               | topList = Board.initialCaptureBoard
                               , bottomList = Board.initialCaptureBoard
                           }
            in
                ( mod3 , Cmd.none )


findValidMoves : Model -> Model
findValidMoves model =
    let moves = Board.validMoves (playerColor model.player) model.board
    in
        case highlightActors moves model of
            Just m ->
                { m | moves = moves }
            Nothing ->
                let player = otherPlayer model.player
                    otherMoves = Board.validMoves (playerColor player) model.board
                    m = { model
                            | player = player
                            , moves = moves
                        }
                in
                    case highlightActors otherMoves m of
                        Nothing ->
                            { m | mode = GameOverMode }
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
                         List.map (\key -> (actorSelectionColor, key)) keys
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

-- TODO
nodeMsg : Model -> NodeMsg
nodeMsg model board node =
    let piece = node.piece
        name = node.name
    in
        case model.mode of
            GameOverMode ->
                Nothing
            SetupMode ->
                let which = case model.player of
                                WhitePlayer -> TopList
                                BlackPlayer -> BottomList
                in
                    if board == model.board then
                        if node.piece == Nothing && model.nodeSelections /= [] then
                            Just <| NodeClick EmptyBoardClick which node
                        else
                            Nothing
                    else if piece /= Nothing then
                        if (model.player == WhitePlayer && board == model.topList) ||
                            (model.player == BlackPlayer && board == model.bottomList)
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
                    Just _ ->
                        case LE.find (\(_, nn) -> name == nn) model.nodeSelections of
                            Nothing ->
                                Nothing
                            _ ->
                                Just <| NodeClick ChooseActorClick MainBoard node
            ChooseSubjectMode ->
                Nothing
            ChooseTargetMode ->
                Nothing

view : Model -> Html Msg
view model =
    let mod = setMessage model
        renderInfo = mod.renderInfo
        cellSize = renderInfo.cellSize
        locations = renderInfo.locations
        listCellSize = if mod.mode == SetupMode then
                           renderInfo.setupCellSize
                       else
                           renderInfo.captureCellSize
        setupLocations = renderInfo.setupLineLocations
        sels = mod.nodeSelections
        (topsel, boardsel, botsel) =
            case mod.mode of
                SetupMode ->
                    case mod.player of
                        WhitePlayer ->
                            (sels, [], [])
                        BlackPlayer ->
                            ([], [], sels)
                _ ->
                    ([], sels, [])
        modNodeMsg = nodeMsg mod
    in
        div [ align "center"
            --deprecated, so sue me
            ]
        [ h2 [] [ text "Archmage" ]
        , p []
            [ case mod.message of
                  Nothing ->
                      text nbsp
                  Just m ->
                      text m
            ]
        , Board.render
            mod.topList setupLocations listCellSize topsel modNodeMsg
        , br
        , Board.render
            mod.board locations cellSize boardsel modNodeMsg
        , br
        , Board.render
            mod.bottomList setupLocations listCellSize botsel modNodeMsg
        , footer
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
