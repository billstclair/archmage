----------------------------------------------------------------------
--
-- DrawPieces.elm
-- Top level test file for piece drawing code
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

module DrawPieces exposing (..)

import Archmage.Types as Types exposing ( Piece(..), Color(..) )
import Archmage.Pieces exposing ( drawPiece )

import Html exposing ( Html, Attribute , div, h2, text )
import Html.Attributes exposing ( align )
import Svg exposing ( Svg, svg, g, rect )
import Svg.Attributes exposing ( x, y, width, height, stroke, strokeWidth, fillOpacity )

type alias Model =
    {
    }

type Msg =
    None

main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\m -> Sub.none)
        }

init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None -> ( model, Cmd.none )

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
            , drawPiece piece color ix iy pieceSize
            ]

view : Model -> Html Msg
view model =
    div
        [ align "center"
          --deprecated, so sue me
        ]
        [ h2 [] [ text "Archmage Pieces" ]
        , svg [ width <| toString (2 + (pieceCount * pieceSize))
              , height <| toString (2 + (2 * pieceSize))
              , stroke "black"
              , strokeWidth "2"
              , fillOpacity "0"
              ]
              <| List.append
                  (List.map2 (onePiece 0) indices pieces)
                  (List.map2 (onePiece 1) indices pieces)
        ]
