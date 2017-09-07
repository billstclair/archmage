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
    , player : Player
    , moves : Maybe MovesDict
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
validActorSelectionColor = "green"
validSubjectSelectionColor = "blue"
validTargetSelectionColor = "red"

messages : List (Mode, String)
messages =
    [ (SetupMode, "select and place a piece.")
    , (ChooseFirstActorMode, "select a " ++ validActorSelectionColor
           ++ "-highighted actor.")
    , (ChooseActorMode, "select a " ++ validActorSelectionColor
           ++ "-highlighted actor, or click the black center square.")
    , (ChooseSubjectMode, "select a " ++ validSubjectSelectionColor
           ++ "-highlighted subject, the actor, or the black center square.")
    , (ChooseTargetMode, "click a " ++ validTargetSelectionColor
           ++ "-highlighted target, the subject, the actor, or the black center square.")
    , (GameOverMode, "Game Over")
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
                              c ++ message
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
              , player = WhitePlayer
              , moves = Nothing
              , nodeSelections = []
              , actor = Nothing
              , subject = Nothing
              , board = Board.initialBoard
              , topList = Board.whiteSetupBoard
              , bottomList = Board.blackSetupBoard
              , renderInfo = Board.renderInfo pieceSize
              , message = Nothing
              }
        mod2 = if not doPlaceAll then
                   { mod
                       | nodeSelections = initialPlacementSelections mod.player mod
                   }
               else
                   findValidMoves
                   { mod
                       | board = Board.dummyBoard
                       , topList = Board.initialCaptureBoard
                       , bottomList = Board.initialCaptureBoard
                       , mode = ChooseFirstActorMode
                   }
        model = setMessage mod2
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
            if kind == SetupBoardClick then
                ( { model
                      | nodeSelections = [ (placementSelectionColor, node.name) ]
                  }
                , Cmd.none
                )
            else if kind == EmptyBoardClick then
                case model.nodeSelections of
                    [(_, sn)] ->
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
                            player =
                                 case which of
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
                                                    ChooseFirstActorMode
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
                           ( setMessage mod3 , Cmd.none )
                    _ ->
                        (model, Cmd.none)
                            
            else
                (model, Cmd.none)
        _ ->
            ( model, Cmd.none )

findValidMoves : Model -> Model
findValidMoves model =
    let moves = Board.validMoves (playerColor model.player) model.board
        printedMoves = log "moves" <| Board.printMoves moves
    in
        case highlightActors moves model of
            Just m ->
                { m | moves = Just moves }
            Nothing ->
                let player = otherPlayer model.player
                    otherMoves = Board.validMoves (playerColor player) model.board
                    m = { model
                            | player = player
                            , moves = Just moves
                        }
                in
                    case highlightActors otherMoves m of
                        Nothing ->
                            setMessage { m | mode = GameOverMode }
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
                         List.map (\key -> (validActorSelectionColor, key)) keys
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
    if model.mode == SetupMode then
        let which = case model.player of
                        WhitePlayer -> TopList
                        BlackPlayer -> BottomList
        in
            if board == model.board then
                if node.piece == Nothing && model.nodeSelections /= [] then
                    Just <| NodeClick EmptyBoardClick which node
                else
                    Nothing
            else if node.piece /= Nothing then
                if (model.player == WhitePlayer && board == model.topList) ||
                    (model.player == BlackPlayer && board == model.bottomList)
                then
                    Just <| NodeClick SetupBoardClick which node
            else
                Nothing
        else
            Nothing
    else
        Nothing

view : Model -> Html Msg
view model =
    let renderInfo = model.renderInfo
        cellSize = renderInfo.cellSize
        locations = renderInfo.locations
        listCellSize = if model.mode == SetupMode then
                           renderInfo.setupCellSize
                       else
                           renderInfo.captureCellSize
        setupLocations = renderInfo.setupLineLocations
        sels = model.nodeSelections
        (topsel, boardsel, botsel) =
            case model.mode of
                SetupMode ->
                    case model.player of
                        WhitePlayer ->
                            (sels, [], [])
                        BlackPlayer ->
                            ([], [], sels)
                _ ->
                    ([], sels, [])
        nm = nodeMsg model
    in
        div [ align "center"
            --deprecated, so sue me
            ]
        [ h2 [] [ text "Archmage" ]
        , p []
            [ case model.message of
                  Nothing ->
                      text nbsp
                  Just m ->
                      text m
            ]
        , Board.render model.topList setupLocations listCellSize topsel nm
        , br
        , Board.render model.board locations cellSize boardsel nm
        , br
        , Board.render model.bottomList setupLocations listCellSize botsel nm
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
