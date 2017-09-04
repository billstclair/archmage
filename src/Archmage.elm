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

import Archmage.Types as Types exposing ( Piece(..), Color(..), Board, RenderInfo
                                        , Msg)
import Archmage.Pieces exposing ( drawPiece )
import Archmage.Board as Board

import Html exposing ( Html, Attribute , div, h2, text, img, p, a )
import Html.Attributes exposing ( align, src, href, target )
import Svg exposing ( Svg, svg, g, rect )
import Svg.Attributes exposing ( x, y, width, height, stroke, strokeWidth, fillOpacity )

type alias Model =
    { board : Board
    , topList : Board
    , bottomList : Board
    , renderInfo : RenderInfo
    }

main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\m -> Sub.none)
        }

init : ( Model, Cmd Msg )
init =
    ( { board = Board.initialBoard
      , topList = Board.whiteSetupBoard
      , bottomList = Board.blackSetupBoard
      , renderInfo = Board.renderInfo pieceSize
      }
    , Cmd.none )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        _ -> ( model, Cmd.none )

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

view : Model -> Html Msg
view model =
    let renderInfo = model.renderInfo
        cellSize = renderInfo.cellSize
        locations = renderInfo.locations
        setupCellSize = renderInfo.setupCellSize
        setupLocations = renderInfo.setupLineLocations
    in
        div [ align "center"
            --deprecated, so sue me
            ]
        [ h2 [] [ text "Archmage" ]
        , Board.render model.topList setupLocations setupCellSize
        , br
        , Board.render model.board locations cellSize
        , br
        , Board.render model.bottomList setupLocations setupCellSize
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
