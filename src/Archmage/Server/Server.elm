port module Server exposing (..)

import Archmage.Server.EncodeDecode exposing ( encodeMessage, decodeMessage )
import Archmage.Server.Interface exposing ( emptyServerState, send
                                          , processServerMessage, errorRsp
                                          )
import Archmage.Server.Error exposing ( ServerError(..), errnum )
import Archmage.Types exposing ( Message(..), ServerState, PublicGames, PlayerNames
                               , Player(..)
                               , noMessage, initialPlayerNames
                               )

import Platform exposing ( Program )
import Json.Decode as Decode exposing ( Decoder )
import Json.Encode as Encode
import Task
import Time exposing ( Time )
import Random exposing ( Seed, Generator ) 
import Char
import Dict exposing ( Dict )
import List.Extra as LE
import Debug exposing ( log )
import WebSocketServer as WSS exposing ( Socket )

main : Program Never Model Msg
main =
  Platform.program
    { init = init
    , update = update
    , subscriptions = subscriptions
    }

-- PORTS

port inputPort : (Decode.Value -> msg) -> Sub msg
port outputPort : Encode.Value -> Cmd msg

-- MODEL

type alias Model =
    { state : ServerState
    , gameidDict : Dict Socket String --Socket -> gameid
    , playeridDict : Dict String (List String) --gameid -> List playerid
    , namesDict : Dict String PlayerNames      --gameid -> PlayerNames
    , socketsDict : Dict String (List Socket) -- gameid -> List Socket
    , seed : Seed
    }

lowercaseLetter : Generator Char
lowercaseLetter =
    Random.map (\n -> Char.fromCode (n + 97)) (Random.int 0 25)

gameidLength : Int
gameidLength =
    16                          --(log (expt 26 16) 2) -> 75

gameidGenerator : Generator String
gameidGenerator =
    Random.map String.fromList
        <| Random.list gameidLength lowercaseLetter

newGameid : Model -> (String, Model)
newGameid model =
    let (res, seed) = Random.step gameidGenerator model.seed
    in
        ( res
        , { model | seed = seed }
        )

newPlayerid : Model -> (String, Model)
newPlayerid model =
    let (gameid, mod) = newGameid model
    in
        ("P" ++ gameid, mod)

init : ( Model, Cmd Msg )
init =
    ( { state = emptyServerState
      , gameidDict = Dict.empty
      , playeridDict = Dict.empty
      , namesDict = Dict.empty
      , socketsDict = Dict.empty
      , seed = Random.initialSeed 0
      }
    , Task.perform Tick Time.now
    )

-- UPDATE

type Msg
  = Connection WSS.Socket
  | Disconnection WSS.Socket
  | SocketMessage Socket String
  | Tick Time
  | Noop

update : Msg -> Model -> (Model, Cmd Msg)
update message model =
  case (Debug.log "Msg" message) of
    Connection socket ->
      ( model, Cmd.none )
    Disconnection socket ->
        disconnection model socket
    SocketMessage socket message ->
        socketMessage model socket message
    Tick time ->
        let seed = Random.initialSeed <| round time
        in
            ( { model | seed = seed }
            , Cmd.none
            )
    Noop -> (model, Cmd.none)

removeField : value -> (record -> value) -> List record -> List record
removeField value accessor records =
    List.filter (\record -> accessor record /= value) records

killGame : Model -> String -> Model
killGame model gameid =
    let state = model.state
        playerDict = case Dict.get (log "killgame" gameid) model.playeridDict of
                         Nothing ->
                             state.playerDict
                         Just ids ->
                             List.foldl Dict.remove state.playerDict ids
    in
        { model
            | state = { state
                          | gameDict = Dict.remove gameid state.gameDict
                          , playerDict = playerDict
                          , publicGames = removeField gameid .gameid state.publicGames
                      }
            , playeridDict = Dict.remove gameid model.playeridDict
        }

disconnection : Model -> Socket -> (Model, Cmd Msg)
disconnection model socket =
    case Dict.get socket model.gameidDict of
        Nothing ->
            (model, Cmd.none)
        Just gameid ->
            let model2 = { model | gameidDict = Dict.remove socket model.gameidDict }
                socketsDict = model.socketsDict
            in
                case Dict.get gameid model.socketsDict of
                    Nothing ->
                        ( model2, Cmd.none )
                    Just sockets ->
                        let socks = List.filter (\s -> s /= socket) sockets
                        in
                            ( if socks == [] then
                                  -- Probably should put a delay here,
                                  -- in case both players lose their connection
                                  -- at the same time, but come back
                                  killGame { model2
                                               | socketsDict =
                                                   Dict.remove gameid socketsDict
                                           }
                                      gameid
                              else
                                  { model2
                                      | socketsDict =
                                          Dict.insert gameid socks socketsDict
                                  }
                            , Cmd.none
                            )

sendToOne : Message -> Socket -> Cmd Msg
sendToOne message socket =
    WSS.sendToOne outputPort
        (log "send" (encodeMessage message))
        (log "  " socket)

sendToMany : Message -> (List Socket) -> Cmd Msg
sendToMany message sockets =
    WSS.sendToMany outputPort
        (log "send" (encodeMessage message))
        (log "  " sockets)
        |> Cmd.batch

messageGameid : Message -> Maybe String
messageGameid message =
    case message of
        NewRsp { gameid } -> Just gameid
        JoinReq { gameid } -> Just gameid
        UpdateRsp { gameid } -> Just gameid
        ChatRsp { gameid } -> Just gameid
        _ -> Nothing

socketMessage : Model -> Socket -> String -> (Model, Cmd Msg)
socketMessage model socket request =
    case decodeMessage request of
        Err msg ->
            let response = ErrorRsp { request = request
                                    , text = "Can't parse request"
                                    }
            in
                ( model
                , sendToOne response socket
                )
        Ok message ->
            let (state, response) = processServerMessage model.state message
            in
                if response == noMessage then
                    ( { model | state = state }
                    , Cmd.none
                    )
                else
                    processResponse model socket state message response

updatePublicGameId : PublicGames -> String -> String -> PublicGames
updatePublicGameId games gameid gid =
    let gameList = games
    in
        case LE.find (\game -> game.gameid == gameid) gameList of
            Nothing ->
                games
            Just game ->
                { game | gameid = gid }
                :: (List.filter (\game -> game.gameid /= gameid) gameList)

updatePlayerid : String -> String -> Player -> Model -> (Model, String)
updatePlayerid gameid playerid player model =
    let (pid, model2) = newPlayerid model
        state = model2.state
        ids = case Dict.get gameid model2.playeridDict of
                  Nothing -> [pid]
                  Just ids -> pid :: ids
        playeridDict = Dict.insert gameid ids model2.playeridDict
        info = { gameid = gameid, player = player }
        playerDict = Dict.insert pid info
                     <| Dict.remove playerid state.playerDict
    in
        ( { model2
              | state = { state | playerDict = playerDict }
              , playeridDict = playeridDict
          }
        , pid
        )

processResponse : Model -> Socket -> ServerState -> Message -> Message -> (Model, Cmd Msg)
processResponse model socket state message response =
    case response of
        (NewRsp { gameid, playerid, name }) ->
            let (model2, _) = disconnection model socket
                (gid, model3) = newGameid model2
                state2 = case Dict.get gameid state.gameDict of
                             Nothing ->
                                 state --can't happen
                             Just gs ->
                                 let gs2 = gs
                                     gameDict =
                                         Dict.remove gameid state.gameDict
                                 in
                                     { state
                                         | gameDict = Dict.insert gid gs2 gameDict
                                         , publicGames =
                                             updatePublicGameId state.publicGames
                                                 gameid gid
                                     }
                model4 = { model3
                             | state = state2
                             , gameidDict =
                                 Dict.insert socket gid model3.gameidDict
                             , socketsDict =
                                 Dict.insert gid [socket] model3.socketsDict
                             , namesDict =
                                 Dict.insert gid
                                     { initialPlayerNames | white = name }
                                     model3.namesDict
                         }
                (model5, pid) = updatePlayerid gid playerid WhitePlayer model4
                response = NewRsp { gameid = gid
                                  , playerid = pid
                                  , name = name
                                  }
            in
                ( model5
                , sendToOne response socket
                )
        JoinRsp { playerid, names, gameState } ->
            case messageGameid message of
                Nothing ->
                    ( model
                    , sendToOne (errorRsp message "Can't find gameid") socket
                    )
                Just gameid ->
                    let (model2, _) = disconnection model socket
                        sockets = case Dict.get gameid model2.socketsDict of
                                      Nothing -> [] --better not happen
                                      Just socks -> socks
                        newSockets = socket :: sockets
                        newNames = case Dict.get gameid model.namesDict of
                                       Nothing -> names
                                       Just nms ->
                                           { nms | black = names.black }
                        model3 = { model2
                                     | state = state
                                     , gameidDict =
                                       Dict.insert socket gameid model2.gameidDict
                                     , socketsDict =
                                       Dict.insert gameid newSockets model2.socketsDict
                                     , namesDict =
                                       Dict.insert gameid newNames model2.namesDict
                                 }
                        (model4, pid) = updatePlayerid gameid playerid BlackPlayer model3
                        rec = { playerid = pid
                              , names = newNames
                              , gameState = gameState
                              }
                        rsp = JoinRsp rec
                        whiteRsp = JoinRsp { rec | playerid = "" }
             in
                 ( model4
                 , Cmd.batch
                     [ sendToOne rsp socket
                     , case sockets of
                           [] ->
                               Cmd.none
                           s :: _ ->
                               sendToOne whiteRsp s
                     ]
                 )
        ErrorRsp _ ->
            ( model
            , sendToOne response socket
            )
        _ ->
            let (gameid, model2) =
                    case Dict.get socket model.gameidDict of
                        Just gid ->
                            (gid, model)
                        Nothing ->
                            case messageGameid response of
                                Nothing ->
                                    ("", model) --I don't think this can happen
                                Just gid ->
                                    let socks =
                                        case Dict.get gid model.socketsDict of
                                            Nothing -> [socket]
                                            Just ss -> socket :: ss
                                    in
                                        ( gid
                                        , { model
                                              | gameidDict =
                                                  Dict.insert socket gid
                                                      model.gameidDict
                                              , socketsDict =
                                                  Dict.insert gid socks
                                                      model.socketsDict
                                          }
                                        )
                sockets = case Dict.get gameid model2.socketsDict of
                              Nothing ->
                                  [socket] --can't happen
                              Just socks ->
                                  socks
            in
                ( { model2 | state = state }
                , sendToMany response sockets
                )

-- SUBSCRIPTIONS

decodeMsg : Decode.Value -> Msg
decodeMsg value =
  let
    decoder = WSS.eventDecoder
      { onConnection = (\socket _ -> Connection socket)
      , onDisconnection = (\socket _ -> Disconnection socket)
      , onMessage = (\socket _ message -> SocketMessage socket message)
      }
  in
    Decode.decodeValue decoder value
      |> Result.withDefault Noop

subscriptions : Model -> Sub Msg
subscriptions model =
    inputPort decodeMsg
