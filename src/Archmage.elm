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
    exposing ( Piece(..), Color(..), ColoredPiece, Board, Node
             , NodeSelection, RenderInfo
             , Page(..), Msg(..), Mode(..), ClickKind(..), WhichBoard(..)
             , NodeMsg, MovesDict
             , setBoardPiece, pieceToAbbreviation
             )
import Archmage.Pieces exposing ( drawPiece )
import Archmage.Board as Board exposing ( getNode, printMove )

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

otherColor : Color -> Color
otherColor color =
    case color of
        White -> Black
        Black -> White

type alias Model =
    { page : Page
    , mode : Mode
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
                                               "."
                                           else
                                               case model.mode of
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
doPlaceAll = False --True

init : ( Model, Cmd Msg )
init =
    let mod = { page = GamePage
              , mode = SetupMode
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
                    case model.mode of
                        SetupMode ->
                            setupEmptyBoardClick which node model
                        _ ->
                            (model, Cmd.none)
                OtherPlayerClick ->
                    ( findValidMoves
                          { model
                              | player = otherPlayer model.player
                              , isFirstMove = True
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
                    in
                        ( { model
                              | mode = ChooseSubjectMode
                              , actor = Just node
                              , nodeSelections =
                                (actorSelectionColor, node.name) :: subjectSelections
                          }
                        , Cmd.none
                        )
                UnchooseActorClick ->
                    ( findValidMoves
                          { model
                              | mode = ChooseActorMode
                              , subject = Nothing
                          }
                    , Cmd.none
                    )
                ChooseSubjectClick ->
                    let actorName = case model.actor of
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
                    in
                        ( { model
                              | mode = ChooseTargetMode
                              , subject = Just node
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
                    case model.actor of
                        Nothing ->
                            (model, Cmd.none) --can't happen
                        Just actor ->
                            update (NodeClick ChooseActorClick MainBoard actor)
                                { model
                                    | mode = ChooseActorMode
                                }
                ChooseTargetClick ->
                    chooseTargetClick node model
        _ ->
            ( model, Cmd.none )

chooseTargetClick : Node -> Model -> (Model, Cmd Msg)
chooseTargetClick target model =
    case model.subject of
        Nothing ->
            (model, Cmd.none)   --can't happen
        Just subject ->
            let b = case model.actor of
                        Nothing ->
                            model.board --can't happen
                        Just actor ->
                            case actor.piece of
                                Nothing ->
                                    model.board --can't happen
                                Just (color, piece) ->
                                    setBoardPiece
                                        actor.name
                                        (Just (otherColor color, piece))
                                        model.board
                b2 = setBoardPiece subject.name Nothing b
                mod = case target.piece of
                          Just _ ->
                              addPieceToCaptureBoard subject.piece
                                  { model | board = b2 }
                          Nothing ->
                              { model |
                                    board =
                                        setBoardPiece
                                            target.name
                                            subject.piece
                                            b2
                              }
            in
                ( findValidMoves
                      { mod
                          | mode = ChooseActorMode
                          , isFirstMove = False
                          , subject = Nothing
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
                            Black -> model.topList
                            White -> model.bottomList
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
            in
                case color of
                    Black ->
                        { model | topList = newBoard }
                    White ->
                        { model | bottomList = newBoard }

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
                    mod = { model
                              | isFirstMove = True
                              , player = player
                              , moves = otherMoves
                          }
                in
                    case highlightActors otherMoves mod of
                        Nothing ->
                            { mod
                                | mode = GameOverMode
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
                             , if model.isFirstMove then
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
                    Just (_, piece) ->
                        case piece of
                            CenterHolePiece ->
                                if model.isFirstMove then
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
        listCellSize = if model.mode == SetupMode then
                           renderInfo.setupCellSize
                       else
                           renderInfo.captureCellSize
        setupLocations = case model.mode of
                             SetupMode -> renderInfo.setupLineLocations
                             _ -> renderInfo.captureLineLocations
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
        modNodeMsg = nodeMsg model
    in
        div []
            [ Board.render
                  model.topList setupLocations listCellSize topsel modNodeMsg
            , br
            , Board.render
                model.board locations cellSize boardsel modNodeMsg
            , br
            , Board.render
                model.bottomList setupLocations listCellSize botsel modNodeMsg
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
