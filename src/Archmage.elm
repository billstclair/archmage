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

import Archmage.Types as Types exposing ( Piece(..), Color(..), Board, Node
                                        , NodeSelection, RenderInfo
                                        , Msg(..), Mode(..), ClickKind(..)
                                        , NodeMsg
                                        , setBoardPiece
                                        )
import Archmage.Pieces exposing ( drawPiece )
import Archmage.Board as Board

import Html exposing ( Html, Attribute , div, h2, text, img, p, a )
import Html.Attributes exposing ( align, src, href, target )
import Svg exposing ( Svg, svg, g, rect )
import Svg.Attributes exposing ( x, y, width, height, stroke, strokeWidth, fillOpacity )
import Char
import Dict exposing ( Dict )
import List.Extra as LE

type Player
    = WhitePlayer
    | BlackPlayer

type alias Model =
    { mode : Mode
    , player : Player
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

messages : List (Mode, String)
messages =
    [ (SetupMode, "select and place a piece.")
    , (ChooseActorMode, "select a green actor, or click the black center square.")
    , (ChooseSubjectMode, "select a blue subject, the actor, or the black center square.")
    , (ChooseTargetMode, "click a red target, the subject, the actor, or the black center square.")
    ]

setMessage : Model -> Model
setMessage model =
    case Types.get model.mode messages of
        Nothing ->
            { model | message = Nothing }
        Just message ->
            let c = case model.player of
                        WhitePlayer -> "White "
                        BlackPlayer -> "Black "
            in
                { model | message = Just <| c ++ message }

placementSelectionColor = "black"
validActorSelectoinColor = "green"
validSubjectSelectionColor = "blue"
validTargetSelectionColor = "red"

initialPlacementSelections : Player -> Model -> List NodeSelection
initialPlacementSelections player model =
    let board = case player of
                    WhitePlayer -> model.topList
                    BlackPlayer -> model.bottomList
        nodes = Dict.toList board.nodes
                |> List.map Tuple.second
                |> List.sortBy .column
    in
        case LE.find (\node -> node.piece /= Nothing) nodes
        of
            Nothing -> []
            Just node -> [ (placementSelectionColor, node) ]

init : ( Model, Cmd Msg )
init =
    let mod = { mode = SetupMode
              , player = WhitePlayer
              , nodeSelections = []
              , actor = Nothing
              , subject = Nothing
              , board = Board.initialBoard
              , topList = Board.whiteSetupBoard
              , bottomList = Board.blackSetupBoard
              , renderInfo = Board.renderInfo pieceSize
              , message = Nothing
              }
        model = setMessage mod
    in
        ( { model | nodeSelections = initialPlacementSelections model.player model }
        , Cmd.none )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NodeClick kind board node ->
            if kind == SetupBoardClick then
                ( { model
                      | nodeSelections = [ (placementSelectionColor, node) ]
                  }
                , Cmd.none
                )
            else if kind == EmptyBoardClick then
                case model.nodeSelections of
                    [(_, sn)] ->
                        let mod = case model.player of
                                      WhitePlayer ->
                                          { model
                                              | topList =
                                                  setBoardPiece
                                                      sn.name Nothing model.topList
                                          }
                                      BlackPlayer ->
                                          { model
                                              | bottomList =
                                                  setBoardPiece
                                                      sn.name Nothing model.bottomList
                                          }
                            player =
                                 case model.player of
                                     WhitePlayer -> BlackPlayer
                                     BlackPlayer -> WhitePlayer
                            selections = initialPlacementSelections player model
                            mod2 = { mod
                                       | board =
                                           setBoardPiece node.name sn.piece mod.board
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

indices : List Int
indices =
    List.range 0 (pieceCount - 1)

onePiece : Int -> Int -> Piece -> Svg Msg
onePiece row col piece =
    let ix = 1 + (col * pieceSize)
        iy = 1 + (row * pieceSize)
        size = toString(pieceSize)
        color = if row == 0 then White else Black
    in
        g []
            [ rect [ x <| toString ix
                   , y <| toString iy
                   , width size
                   , height size
                   ]
                  []
            , drawPiece piece color (ix+1) (iy+1) (pieceSize-2)
            ]

drawPieceRows : Svg Msg
drawPieceRows =
    svg [ width <| toString (2 + (pieceCount * pieceSize))
        , height <| toString (2 + (2 * pieceSize))
        , stroke "black"
        , strokeWidth "2"
        , fillOpacity "0"
        ]
    <| List.append
        (List.map2 (onePiece 0) indices pieces)
        (List.map2 (onePiece 1) indices pieces)


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
        if board == model.board then
            if node.piece == Nothing && model.nodeSelections /= [] then
                Just <| NodeClick EmptyBoardClick board node
            else
                Nothing
        else if node.piece /= Nothing then
            if (model.player == WhitePlayer && board == model.topList) ||
               (model.player == BlackPlayer && board == model.bottomList)
            then
                Just <| NodeClick SetupBoardClick board node
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
        selections = model.nodeSelections
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
        , Board.render model.topList setupLocations listCellSize selections nm
        , br
        , Board.render model.board locations cellSize selections nm
        , br
        , Board.render model.bottomList setupLocations listCellSize selections nm
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
