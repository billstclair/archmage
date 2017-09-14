----------------------------------------------------------------------
--
-- EncodeDecode.elm
-- JSON encoder and decoder for Archmage server wire protocol.
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

module Archmage.Server.EncodeDecode 
    exposing ( encodeGameState, decodeGameState, restoreGame
             , fixCurlyQuotes
             )

import Archmage.Types as Types
    exposing ( GameState, Color(..), Piece(..), Board, Node
             , ColoredPiece, Player(..), TheGameState(..)
             , GameAnalysis, emptyAnalysis
             , Mode(..), pieceToString, stringToPiece
             , get, rget
             )
import Archmage.Board as Board
    exposing ( boardToString, stringToBoard )
        
import Json.Decode as JD exposing ( Decoder )
import Json.Encode as JE exposing ( Value )
import Char
import String.Extra as SE

---
--- Decoder
---

restoreGame : String -> Result String GameState
restoreGame restoreString =
    case decodeGameState restoreString of
        Ok gs ->
            Ok <| Board.addAnalysis gs
        Err msg ->
            Err msg

decodeGameState : String -> Result String GameState
decodeGameState string =
    JD.decodeString gameStateDecoder string

type alias Boards =
    { board : Board
    , topList : Board
    , bottomList : Board
    }

makeGameState : Player -> Mode -> Bool -> Maybe Node -> Maybe Node -> Boards -> List String -> List TheGameState -> GameState
makeGameState player mode isFirstMove actor subject {board, topList, bottomList} history turnMoves =
    GameState player mode isFirstMove actor subject board topList bottomList history turnMoves emptyAnalysis

type alias Encoder a =
    Value -> Result String a

andThenSucceed : JD.Decoder a -> Value -> JD.Decoder a
andThenSucceed decoder value =
    case JD.decodeValue decoder value of
        Ok a ->
            JD.succeed a
        Err msg ->
            JD.fail msg

gameStateDecoder : Decoder GameState
gameStateDecoder =
    JD.andThen
        (\list ->
             case list of
                 [ player, mode, isFirstMove, actor, subject, boards, history, turnMoves] ->
                     JD.map8
                         makeGameState
                         (andThenSucceed playerDecoder player)
                         (andThenSucceed modeDecoder mode)
                         (andThenSucceed JD.bool isFirstMove)
                         (andThenSucceed (JD.nullable nodeDecoder) actor)
                         (andThenSucceed (JD.nullable nodeDecoder) subject)
                         (andThenSucceed boardsDecoder boards)
                         (andThenSucceed (JD.list JD.string) history)
                         (andThenSucceed (JD.list theGameStateDecoder) turnMoves)
                 _ ->
                     JD.fail "Other than 8 elements in GameState list."
        )
        (JD.list JD.value)

playerDecoder : Decoder Player
playerDecoder =
    JD.andThen
        (\s ->
             case stringToPlayer s of
                 Ok player ->
                     JD.succeed player
                 Err msg ->
                     JD.fail msg
        )
        JD.string

modeDecoder : Decoder Mode
modeDecoder =
    JD.andThen
        (\s ->
             case stringToMode s of
                 Ok mode ->
                     JD.succeed mode
                 Err msg ->
                     JD.fail msg
        )
        JD.string

nodeDecoder : Decoder Node
nodeDecoder =
    JD.andThen (\list ->
                    case list of
                        [name, row, column, piece] ->
                            JD.map4 Node
                                (andThenSucceed JD.string name)
                                (andThenSucceed JD.int row)
                                (andThenSucceed JD.int column)
                                (andThenSucceed (JD.nullable coloredPieceDecoder)
                                     piece)
                        _ ->
                            JD.fail "Other than 4 elements in an encoded Node."
               )
        (JD.list JD.value)

coloredPieceDecoder : Decoder ColoredPiece
coloredPieceDecoder =
    JD.andThen (\list ->
                    case list of
                        [color, piece] ->
                            JD.map2 (,)
                                (andThenSucceed colorDecoder color)
                                (andThenSucceed pieceDecoder piece)
                        _ ->
                            JD.fail "Other than 2 elements in an encoded ColoredPiece."
               )
        (JD.list JD.value)

colorDecoder : Decoder Color
colorDecoder =
    JD.andThen
        (\string ->
             case stringToColor string of
                 Ok s ->
                     JD.succeed s
                 Err msg ->
                     JD.fail msg
        )
        JD.string

pieceDecoder : Decoder Piece
pieceDecoder =
    JD.map stringToPiece JD.string

boardsDecoder : Decoder Boards
boardsDecoder =
    JD.andThen
        (\list ->
             case List.map stringToBoard list of
                 [b, tl, bl] ->
                     JD.succeed <| Boards b tl bl
                 _ ->
                     JD.fail "Wrong number of boards."
        )
        (JD.list JD.string)

boardDecoder : Decoder Board
boardDecoder =
    JD.map stringToBoard JD.string

theGameStateDecoder : Decoder TheGameState
theGameStateDecoder =
    JD.lazy (\() -> JD.map TheGameState gameStateDecoder)

---
--- Encoder
---

encodeGameState : GameState -> String
encodeGameState gs =
    JE.encode 0 <| gameStateEncoder gs

getTheGameState : TheGameState -> GameState
getTheGameState (TheGameState gs) =
    gs

gameStateEncoder : GameState -> Value
gameStateEncoder gs =
    JE.list
        [ JE.string <| playerToString gs.player
        , JE.string <| modeToString gs.mode
        , JE.bool <| gs.isFirstMove
        , encodeMaybeNode gs.actor
        , encodeMaybeNode gs.subject
        , JE.list
            [ JE.string <| boardToString gs.board
            , JE.string <| boardToString gs.topList
            , JE.string <| boardToString gs.bottomList
            ]
        , JE.list <| List.map JE.string gs.history
        , JE.list
              <| List.map (gameStateEncoder << getTheGameState) gs.turnMoves
        ]

encodeMaybeNode : Maybe Node -> Value
encodeMaybeNode node =
    case node of
        Nothing ->
            JE.null
        Just n ->
            encodeNode n

encodeNode : Node -> Value
encodeNode node =
    JE.list
        [ JE.string node.name
        , JE.int node.row
        , JE.int node.column
        , case node.piece of
              Nothing ->
                  JE.null
              Just piece ->
                  encodeColoredPiece piece
        ]
        
encodeColoredPiece : ColoredPiece -> Value
encodeColoredPiece (color, piece) =
    JE.list
        [ JE.string <| colorToString color
        , JE.string <| pieceToString piece
        ]
        
---
--- Primitives
---

playerToString : Player -> String
playerToString player =
    case player of
        WhitePlayer ->
            "WP"
        BlackPlayer ->
            "BP"

stringToPlayer : String -> Result String Player
stringToPlayer string =
    if string == "WP" then
        Ok WhitePlayer
    else if string == "BP" then
        Ok BlackPlayer
    else
        Err <| "Not a player: " ++ string

colorToString : Color -> String
colorToString color =
    case color of
        White ->
            "W"
        Black ->
            "B"

stringToColor : String -> Result String Color
stringToColor string =
    if string == "W" then
        Ok White
    else if string == "B" then
        Ok Black
    else
        Err <| "Not a color: " ++ string

modePlist : List (String, Mode)
modePlist =
    [ ("S", SetupMode)
    , ("CA", ChooseActorMode)
    , ("CS", ChooseSubjectMode)
    , ("CT", ChooseTargetMode)
    , ("GO", GameOverMode)
    ]

modeToString : Mode -> String
modeToString mode =
    case rget mode modePlist of
        Just s ->
            s
        Nothing ->
            "S" --can't happen

stringToMode : String -> Result String Mode
stringToMode string =
    case get string modePlist of
        Just mode ->
            Ok mode
        Nothing ->
            Err <| "Not a mode: " ++ string    

leftCurlyQuote : String
leftCurlyQuote =
    String.fromChar(Char.fromCode(8220))

rightCurlyQuote : String
rightCurlyQuote =
    String.fromChar(Char.fromCode(8221))

asciiQuote : String
asciiQuote =
    "\""

-- Users will sometimes type restore state strings, so they'll get
-- curly quotes, thanks to helpful text editors.
fixCurlyQuotes : String -> String
fixCurlyQuotes string =
    SE.replace leftCurlyQuote asciiQuote
        <| SE.replace rightCurlyQuote asciiQuote string
