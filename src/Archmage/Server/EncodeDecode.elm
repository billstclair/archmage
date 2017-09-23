---------------------------------------------------------------------
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
    exposing ( encodeModel, decodeModel
             , encodeGameState, decodeGameState, restoreGame
             , decodeMessage, encodeMessage, modeToString
             , fixCurlyQuotes
             )

import Archmage.Types as Types
    exposing ( Model, GameState, Color(..), Piece(..), Board, Node
             , NodeSelection, Page(..), ServerInterface(..), Msg(..)
             , ColoredPiece, Player(..), TheGameState(..), ServerState
             , PlayerInfo, GameAnalysis, emptyAnalysis
             , Mode(..), pieceToString, stringToPiece
             , Message(..), PublicGame, PublicGames, PlayerNames
             , get, rget
             )
import Archmage.Board as Board
    exposing ( boardToString, stringToBoard, pieceToChar, charToPiece )
        
import Json.Decode as JD exposing ( Decoder )
import Json.Encode as JE exposing ( Value )
import Char
import String.Extra as SE
import Dict exposing ( Dict )

---
--- Decoders
---

-- Model

decodeModel : String -> Result String Model
decodeModel json =
    JD.decodeString modelDecoder json

--    Err "decodeModel not yet implemented."
  

modelDecoder : Decoder Model
modelDecoder =
    JD.map8 makeSavedModel
        (JD.field "page" pageDecoder)
        (JD.field "GameID" gameIDDecoder)
        (JD.field "remoteType" JD.string)
        (JD.field "names" playerNamesDecoder)
        (JD.field "nodeSelections" (JD.list nodeSelectionDecoder))
        (JD.field "message" (JD.nullable JD.string))
        (JD.field  "gs" gameStateDecoder)
        (JD.field "server" serverInterfaceDecoder)

-- These are put together only to enable JD.map8, which is the maximum
-- number of args to a map function. Yes, I know the alternative.
type alias GameID =
    { gameid : String
    , playerid : String
    , you : Player
    }

gameIDDecoder : Decoder GameID
gameIDDecoder =
    JD.map3 GameID
        (JD.field "gameid" JD.string)
        (JD.field "playerid" JD.string)
        (JD.field "you" playerDecoder)

makeSavedModel : Page -> GameID -> String -> PlayerNames -> List NodeSelection -> Maybe String -> GameState -> ServerInterface Msg -> Model
makeSavedModel page {gameid, playerid, you} remoteType names nodeSelections message gs server =
    let (isRemote, isPublic) = parseRemoteType remoteType
    in
        { page = page
        , nodeSelections = nodeSelections
        , message = message
        , gs = gs
        , isRemote = isRemote
        , isPublic = isPublic
        , server = server
        , gameid = gameid
        , playerid = playerid
        , you = you
        , names = names
        , restoreState = ""
        , windowSize = Nothing
        , renderInfo = Nothing
        , newIsRemote = isRemote
        , otherPlayerid = ""
        }

makePage : String -> Decoder Page
makePage string =
    case string of
        "gamePage" -> JD.succeed GamePage
        "publicPage" -> JD.succeed PublicPage
        "rulesPage" -> JD.succeed RulesPage
        "helpPage" -> JD.succeed HelpPage
        _ -> JD.fail <| "Unknown page name: " ++ string

pageDecoder : Decoder Page
pageDecoder =
    JD.andThen makePage JD.string
        
nodeSelectionDecoder : Decoder NodeSelection
nodeSelectionDecoder =
    JD.map2 (,)
        (JD.field "color" JD.string)
        (JD.field "node" JD.string)

makeServerInterface : String -> ServerInterface Msg
makeServerInterface server =
    ServerInterface
    { server = server
    , wrapper = (\_ _ -> Noop)
    , state = Nothing
    , sender = (\_ _ -> Cmd.none)
    }

serverInterfaceDecoder : Decoder (ServerInterface Msg)
serverInterfaceDecoder =
    JD.map makeServerInterface
        JD.string

-- GameState

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

makeGameState : Player -> Mode -> Bool -> Maybe Node -> Maybe Node -> Boards -> List String -> Maybe TheGameState -> GameState
makeGameState player mode isFirstMove actor subject {board, topList, bottomList} history undoState =
    GameState player mode isFirstMove actor subject board topList bottomList history undoState emptyAnalysis

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
                 [ player, mode, isFirstMove, actor, subject, boards, history, undoState ] ->
                     JD.map8
                         makeGameState
                         (andThenSucceed playerDecoder player)
                         (andThenSucceed modeDecoder mode)
                         (andThenSucceed JD.bool isFirstMove)
                         (andThenSucceed (JD.nullable nodeDecoder) actor)
                         (andThenSucceed (JD.nullable nodeDecoder) subject)
                         (andThenSucceed boardsDecoder boards)
                         (andThenSucceed historyDecoder history)
                         (andThenSucceed (JD.nullable theGameStateDecoder) undoState)
                 _ ->
                     JD.fail "Other than 8 elements in GameState list."
        )
        (JD.list JD.value)

isValidGameboardString : String -> Bool
isValidGameboardString string =
    49 == (List.length <| Board.runLengthDecode (String.toList string))

historyDecoder : Decoder (List String)
historyDecoder =
    JD.andThen
        (\histories ->
             let loop = (\hs ->
                             case hs of
                                 [] ->
                                     JD.succeed histories
                                 h :: tail ->
                                     if isValidGameboardString h then
                                         loop tail
                                     else
                                         JD.fail
                                             <| "Not a valid game board string: " ++ h
                        )
             in
                 loop histories
        )
        (JD.list JD.string)

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

decodeColoredPiece : String -> Result String ColoredPiece
decodeColoredPiece string =
    JD.decodeString coloredPieceDecoder string

coloredPieceDecoder : Decoder ColoredPiece
coloredPieceDecoder =
    JD.andThen (\string ->
                    case String.toList string of
                        [piece] ->
                            case charToPiece piece of
                                Nothing ->
                                    JD.fail "Blank piece"
                                Just p ->
                                    JD.succeed p
                        _ ->
                            JD.fail "Other than 2 characters in an encoded ColoredPiece."
               )
        JD.string

decodePlayerNames : String -> Result String PlayerNames
decodePlayerNames string =
    JD.decodeString playerNamesDecoder string

playerNamesDecoder : Decoder PlayerNames
playerNamesDecoder =
    JD.map2 PlayerNames
        (JD.field "white" JD.string)
        (JD.field "black" JD.string)

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

decodePublicGames : String -> Result String PublicGames
decodePublicGames string =
    JD.decodeString publicGamesDecoder string

publicGamesDecoder : Decoder PublicGames
publicGamesDecoder =
    JD.list publicGameDecoder

publicGameDecoder : Decoder PublicGame
publicGameDecoder =
    JD.andThen
        (\list ->
             case list of
                 [ gameid, playerName ] ->
                     JD.succeed
                         { gameid = gameid
                         , playerName = playerName
                         }
                 _ ->
                     JD.fail "Not list of gameid and player name."
        )
        (JD.list JD.string)

isValidGameBoard : Board -> Bool
isValidGameBoard board =
    board.rows == 7 && board.cols == 7

areValidTopAndBottomBoards : Board -> Board -> Bool
areValidTopAndBottomBoards top bottom =
    top.rows == 1 && bottom.rows == 1 &&
    (top.cols == 7 || top.cols == 13) &&
    top.cols == bottom.cols

boardsDecoder : Decoder Boards
boardsDecoder =
    JD.andThen
        (\list ->
             case List.map stringToBoard list of
                 [b, tl, bl] ->
                     if isValidGameBoard b &&
                         areValidTopAndBottomBoards tl bl then
                         JD.succeed <| Boards b tl bl
                     else
                         JD.fail "Wrong board sizes."
                 _ ->
                     JD.fail "Wrong number of boards."
        )
        (JD.list JD.string)

theGameStateDecoder : Decoder TheGameState
theGameStateDecoder =
    JD.lazy (\() -> JD.map TheGameState gameStateDecoder)

-- Message

decodeMessage : String -> Result String Message
decodeMessage string =
    JD.decodeString messageDecoder string

messageDecoder : Decoder Message
messageDecoder =
    JD.map parseRawMessage rawMessageDecoder

rawMessageDecoder : Decoder Message
rawMessageDecoder =
    JD.map3 RawMessage
        (JD.index 0 JD.string)
        (JD.index 1 JD.string)
        (JD.index 2 (JD.keyValuePairs JD.string))

type alias MessageParams =
    { req : Maybe String
    , rsp : Maybe String
    , name : Maybe String
    , names : Maybe PlayerNames
    , isPublic : Bool
    , restoreState : Maybe GameState
    , gameid : Maybe String
    , playerid : Maybe String
    , you : Maybe Player
    , player : Maybe Player
    , gameState : Maybe GameState
    , text : Maybe String
    , piece : Maybe ColoredPiece
    , node : Maybe String
    , request : Maybe String
    , games : Maybe PublicGames
    }

rawMessageToParams : Message -> Maybe MessageParams
rawMessageToParams message =
    case message of
        RawMessage typ msg plist ->
            Just { req = if typ == "req" then Just msg else Nothing
                 , rsp = if typ == "rsp" then Just msg else Nothing
                 , name = get "name" plist
                 , names = maybePlayerNames
                           <| get "names" plist
                 , isPublic = Maybe.withDefault False
                              <| maybeBool (get "isPublic" plist)
                 , gameid = get "gameid" plist
                 , playerid = get "playerid" plist
                 , you = maybePlayer
                         <| get "you" plist
                 , player = maybePlayer
                            <| get "player" plist
                 , gameState = maybeGameState
                               <| get "gameState" plist
                 , text = get "text" plist
                 , restoreState = maybeGameState
                                  <| get "restoreState" plist
                 , piece = maybeColoredPiece
                           <| get "piece" plist
                 , node = get "node" plist
                 , request = get "request" plist
                 , games = maybePublicGames
                           <| get "games" plist
                 }
        _ ->
            Nothing

parseRawMessage : Message -> Message
parseRawMessage rawMessage =
    case rawMessageToParams rawMessage of
        Nothing ->
            rawMessage
        Just params ->
            let { req, rsp } = params
            in
                case (req, rsp) of
                    (Just msg, Nothing) ->
                        parseRequest msg params rawMessage
                    (Nothing, Just msg) ->
                        parseResponse msg params rawMessage
                    _ ->
                        rawMessage

parseRequest : String -> MessageParams -> Message -> Message
parseRequest msg params rawMessage =
    case msg of
        -- New game
        "new" ->
            case params.name of
                Nothing ->
                    rawMessage
                Just n ->
                    NewReq { name = n
                           , isPublic = params.isPublic
                           , restoreState = params.restoreState
                           }
        "join" ->
            case params.gameid of
                Nothing ->
                    rawMessage
                Just gid ->
                    case params.name of
                        Nothing ->
                            rawMessage
                        Just n ->
                            JoinReq { gameid = gid
                                    , name = n
                                    }
        "update" ->
            case params.playerid of
                Nothing ->
                    rawMessage
                Just pid ->
                    UpdateReq { playerid = pid }
        -- Placement
        "selectPlacement" ->
            case params.playerid of
                Nothing ->
                    rawMessage
                Just pid ->
                    case params.node of
                        Nothing ->
                            rawMessage
                        Just n ->
                            SelectPlacementReq { playerid = pid
                                               , node = n
                                               }
        "place" ->
            case params.playerid of
                Nothing ->
                    rawMessage
                Just pid ->
                    case params.node of
                        Nothing ->
                            rawMessage
                        Just n ->
                            PlaceReq { playerid = pid
                                     , node = n
                                     }
        -- Game play
        "selectActor" ->
            case params.playerid of
                Nothing ->
                    rawMessage
                Just pid ->
                    case params.node of
                        Nothing ->
                            rawMessage
                        Just n ->
                            SelectActorReq { playerid = pid
                                           , node = n
                                           }
        "selectSubject" ->
            case params.playerid of
                Nothing ->
                    rawMessage
                Just pid ->
                    case params.node of
                        Nothing ->
                            rawMessage
                        Just n ->
                            SelectSubjectReq { playerid = pid
                                             , node = n
                                             }
        "move" ->
            case params.playerid of
                Nothing ->
                    rawMessage
                Just pid ->
                    case params.node of
                        Nothing ->
                            rawMessage
                        Just n ->
                            MoveReq { playerid = pid
                                    , node = n
                                    }
        "endTurn" ->
            case params.playerid of
                Nothing ->
                    rawMessage
                Just pid ->
                    EndTurnReq { playerid = pid }
        -- Public games
        "games" ->
            GamesReq
        -- Errors
        "undo" ->
            case params.playerid of
                Nothing ->
                    rawMessage
                Just pid ->
                    UndoReq { playerid = pid }
        -- Chat
        "chat" ->
            case params.playerid of
                Nothing ->
                    rawMessage
                Just pid ->
                    case params.text of
                        Nothing ->
                            rawMessage
                        Just t ->
                            ChatReq { playerid = pid
                                    , text = t
                                    }
        _ ->
            rawMessage

parseResponse : String -> MessageParams -> Message -> Message
parseResponse msg params rawMessage =
    case msg of
        -- New game
        "new" ->
            case params.gameid of
                Nothing ->
                    rawMessage
                Just gid ->
                    case params.playerid of
                        Nothing ->
                            rawMessage
                        Just pid ->
                            case params.name of
                                Nothing ->
                                    rawMessage
                                Just n ->
                                    NewRsp { gameid = gid
                                           , playerid = pid
                                           , name = n
                                           }
        "join" ->
            case params.playerid of
                Nothing ->
                    rawMessage
                Just pid ->
                    case params.names of
                        Nothing ->
                            rawMessage
                        Just ns ->
                            case params.gameState of
                                Nothing ->
                                    rawMessage
                                Just gs ->
                                    JoinRsp { playerid = pid
                                            , names = ns
                                            , gameState = gs
                                            }
        -- Generic responses
        "update" ->
            case params.gameid of
                Nothing ->
                    rawMessage
                Just gid ->
                    case params.gameState of
                        Nothing ->
                            rawMessage
                        Just gs ->
                            UpdateRsp { gameid = gid
                                      , gameState = gs
                                      }
        -- Public Games
        "games" ->
            case params.games of
                Nothing ->
                    rawMessage
                Just games ->
                    GamesRsp games
        -- Errors
        "error" ->
            case params.request of
                Nothing ->
                    rawMessage
                Just req ->
                    case params.text of
                        Nothing ->
                            rawMessage
                        Just t ->
                            ErrorRsp { request = req
                                     , text = t
                                     }
        "chat" ->
            case params.gameid of
                Nothing ->
                    rawMessage
                Just gid ->
                    case params.player of
                        Nothing ->
                            rawMessage
                        Just p ->
                            case params.text of
                                Nothing ->
                                    rawMessage
                                Just t ->
                                    ChatRsp { gameid = gid
                                            , player = p
                                            , text = t
                                            }
        _ ->
            rawMessage

---
--- Encoders
---

-- Model

encodeModel : Model -> String
encodeModel model =
    JE.encode 0 <| modelEncoder model

modelEncoder : Model -> Value
modelEncoder model =
    JE.object
        [ ("page", pageEncoder model.page)
        , ("GameID", gameIDEncoder { gameid =  model.gameid
                                   , playerid = model.playerid
                                   , you = model.you
                                   }
          )
        , ("remoteType"
          , JE.string
              <| encodeRemoteType model.isRemote model.isPublic
          )
        , ("names", playerNamesEncoder model.names)
        , ("nodeSelections"
          , JE.list
              <| List.map nodeSelectionEncoder model.nodeSelections
          )
        , ("message", case model.message of
                          Nothing -> JE.null
                          Just m -> JE.string m
          )
        , ("gs", gameStateEncoder model.gs)
        , ("server", serverInterfaceEncoder model.server)
        ]

gameIDEncoder : GameID -> Value
gameIDEncoder id =
    JE.object [ ("gameid", JE.string id.gameid)
              , ("playerid", JE.string id.playerid)
              , ("you", JE.string <| playerToString id.you)
              ]

pageEncoder : Page -> Value
pageEncoder page =
    JE.string
        <| case page of
               GamePage -> "gamePage"
               PublicPage -> "publicPage"
               RulesPage -> "rulesPage"
               HelpPage -> "helpPage"

nodeSelectionEncoder : NodeSelection -> Value
nodeSelectionEncoder (color, node) =
    JE.object [ ("color", JE.string color)
              , ("node", JE.string node)
              ]

serverInterfaceEncoder : ServerInterface Msg -> Value
serverInterfaceEncoder (ServerInterface server) =
    JE.string server.server

-- GameState

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
        , maybeNodeEncoder gs.actor
        , maybeNodeEncoder gs.subject
        , JE.list
            [ JE.string <| boardToString gs.board
            , JE.string <| boardToString gs.topList
            , JE.string <| boardToString gs.bottomList
            ]
        , JE.list <| List.map JE.string gs.history
        , case gs.undoState of
              Nothing ->
                  JE.null
              Just (TheGameState gs) ->
                  gameStateEncoder gs
        ]

maybeNodeEncoder : Maybe Node -> Value
maybeNodeEncoder node =
    case node of
        Nothing ->
            JE.null
        Just n ->
            nodeEncoder n

nodeEncoder : Node -> Value
nodeEncoder node =
    JE.list
        [ JE.string node.name
        , JE.int node.row
        , JE.int node.column
        , case node.piece of
              Nothing ->
                  JE.null
              Just piece ->
                  coloredPieceEncoder piece
        ]
        
encodeColoredPiece : ColoredPiece -> String
encodeColoredPiece piece =
    JE.encode 0 <| coloredPieceEncoder piece

coloredPieceEncoder : ColoredPiece -> Value
coloredPieceEncoder piece =
    JE.string
        (String.fromChar <| pieceToChar (Just piece))
        
encodePlayerNames : PlayerNames -> String
encodePlayerNames names =
    JE.encode 0 <| playerNamesEncoder names

playerNamesEncoder : PlayerNames -> Value
playerNamesEncoder names =
    JE.object [ ("white", JE.string names.white)
              , ("black", JE.string names.black)
              ]

encodePublicGame : PublicGame -> String
encodePublicGame game =
    JE.encode 0 <| publicGameEncoder game

publicGameEncoder : PublicGame -> Value
publicGameEncoder game =
    JE.list
        [ JE.string game.gameid
        , JE.string game.playerName
        ]

encodePublicGames : PublicGames -> String
encodePublicGames games =
    JE.encode 0 <| JE.list (List.map publicGameEncoder games)

-- Message

encodeMessage : Message -> String
encodeMessage message =
    JE.encode 0 <| messageEncoder message

messageValue : String -> String -> List (String, String) -> Value
messageValue typ msg params =
    let p = List.map (\(k, v) -> (k, JE.string v)) params
    in
        JE.list [ JE.string typ, JE.string msg, JE.object p ]

messageEncoder : Message -> Value
messageEncoder message =
    case message of
        RawMessage typ msg plist ->
            messageValue typ msg plist
        -- New game
        NewReq { name, isPublic, restoreState } ->
            let isPublicPairs =
                    if isPublic then
                        [ ("isPublic", "true") ]
                    else
                        []
                rsPairs = case restoreState of
                              Nothing ->
                                  []
                              Just gs ->
                                  [ ("restoreState", encodeGameState gs) ]
            in
                messageValue "req" "new"
                    <| List.concat
                        [ [ ("name", name) ]
                        , isPublicPairs
                        , rsPairs
                        ]
        NewRsp { gameid, playerid, name } ->
            messageValue "rsp" "new"
                [ ("gameid", gameid)
                , ("playerid", playerid)
                , ("name", name) ]
        JoinReq { gameid, name } ->
            messageValue "req" "join" [ ("gameid", gameid), ("name", name) ]
        UpdateReq { playerid } ->
            messageValue "req" "update" [ ("playerid", playerid) ]
        JoinRsp { playerid, names, gameState } ->
            messageValue "rsp" "join"
                [ ("playerid", playerid)
                , ("names", encodePlayerNames names)
                , ("gameState", encodeGameState gameState)
                ]
        -- Generic respones
        UpdateRsp { gameid, gameState } ->
            messageValue "rsp" "update"
                [ ("gameid", gameid)
                , ("gameState", encodeGameState gameState)
                ]
        -- Placement
        SelectPlacementReq { playerid, node } ->
            messageValue "req" "selectPlacement"
                [ ("playerid", playerid)
                , ("node", node)
                ]
        PlaceReq { playerid, node } ->
            messageValue "req" "place"
                [ ("playerid", playerid)
                , ("node", node)
                ]
        -- Game play
        SelectActorReq { playerid, node } ->
            messageValue "req" "selectActor"
                [ ("playerid", playerid)
                , ("node", node)
                ]
        SelectSubjectReq { playerid, node } ->
            messageValue "req" "selectSubject"
                [ ("playerid", playerid)
                , ("node", node)
                ]
        MoveReq { playerid, node } ->
            messageValue "req" "move"
                [ ("playerid", playerid)
                , ("node", node)
                ]
        EndTurnReq { playerid } ->
            messageValue "req" "endTurn"
                [ ("playerid", playerid)
                ]
        -- Public games
        GamesReq ->
            messageValue "req" "games"
                []
        GamesRsp games ->
            messageValue "rsp" "games"
                [ ("games", encodePublicGames games) ]
        -- Errors
        UndoReq { playerid } ->
            messageValue "req" "undo" [ ("playerid", playerid) ]
        ErrorRsp { request, text } ->
            messageValue "rsp" "error"
                [ ("request", request)
                , ("text", text)
                ]
        -- Chat
        ChatReq { playerid, text } ->
            messageValue "req" "chat"
                [ ("playerid", playerid)
                , ("text", text)
                ]
        ChatRsp { gameid, player, text } ->
            messageValue "rsp" "chat"
                [ ("gameid", gameid)
                , ("player", playerToString player)
                , ("text", text)
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
    [ ("J", JoinMode)
    , ("S", SetupMode)
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

maybeBool : Maybe String -> Maybe Bool
maybeBool mb =
    case mb of
        Nothing ->
            Nothing
        Just s ->
            case s of
                "true" ->
                    Just True
                "false" ->
                    Just False
                _ ->
                    Nothing

maybeGameState : Maybe String -> Maybe GameState
maybeGameState string =
    case string of
        Nothing ->
            Nothing
        Just s ->
            case decodeGameState s of
                Ok gs ->
                    Just gs
                Err _ ->
                    Nothing

maybeColoredPiece : Maybe String -> Maybe ColoredPiece
maybeColoredPiece string =
    case string of
        Nothing ->
            Nothing
        Just s ->
            case decodeColoredPiece s of
                Ok piece ->
                    Just piece
                Err _ ->
                    Nothing

maybePlayer : Maybe String -> Maybe Player
maybePlayer string =
    case string of
        Nothing ->
            Nothing
        Just s ->
            case stringToPlayer s of
                Ok player ->
                    Just player
                Err _ ->
                    Nothing

maybePlayerNames : Maybe String -> Maybe PlayerNames
maybePlayerNames string =
    case string of
        Nothing ->
            Nothing
        Just s ->
            case decodePlayerNames s of
                Ok names ->
                    Just names
                Err _ ->
                    Nothing

maybePublicGames : Maybe String -> Maybe PublicGames
maybePublicGames string =
    case string of
        Nothing ->
            Nothing
        Just s ->
            case decodePublicGames s of
                Ok games ->
                    Just games
                Err _ ->
                    Nothing

parseRemoteType : String -> (Bool, Bool)
parseRemoteType string =
    case string of
        "R" -> (True, False)
        "P" -> (True, True)
        _ -> (False, False)

encodeRemoteType : Bool -> Bool -> String
encodeRemoteType isRemote isPublic =
    if isRemote then
        if isPublic then
            "P"
        else
            "R"
    else
        "L"
