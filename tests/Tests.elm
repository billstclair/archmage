module Tests exposing (all)

import Test exposing (..)
import Expect exposing ( Expectation )
import List
import Dict
import Maybe exposing ( withDefault )

import Archmage.Server.EncodeDecode as ED
import Archmage.Types as Types exposing ( Message(..)
                                        , Player(..), Color(..), Piece(..)
                                        )
import Archmage.Board as Board exposing (stringToBoard, boardToString)

log = Debug.log

enableLogging : Bool
enableLogging =
  False                         --change to True to log JSON input & output results

maybeLog : String -> a -> a
maybeLog label value =
  if enableLogging then
    log label value
  else
    value

testMap : (x -> String -> Test) -> List x -> List Test
testMap test data =
    let numbers = List.map toString <| List.range 1 (List.length data)
    in
        List.map2 test data numbers

all : Test
all =
    Test.concat <|
        List.concat
            [ (testMap protocolTest protocolData)
            , (testMap boardTest boardData)
            , (testMap gameStateTest gameStateData)
            ]

expectResult : Result String Message -> Result String Message -> Expectation
expectResult sb was =
    case (maybeLog "  result" was) of
        Err msg ->
            case sb of
                Err _ ->
                    Expect.true "You shouldn't ever see this." True
                Ok _ ->
                    Expect.false msg True
        Ok wasv ->
            case sb of
                Err _ ->
                    Expect.false "Expected an error but didn't get one." True
                Ok sbv ->
                    Expect.equal sbv wasv

protocolTest : Message -> String -> Test
protocolTest message name =
    test ("protocolTest \"" ++ name ++ "\"")
        (\_ ->
             let json = maybeLog "protocolJson" <| ED.encodeMessage message
             in
                 expectResult (Ok message) <| ED.decodeMessage json
        )

protocolData : List Message
protocolData =
    [ RawMessage "foo" "bar" [("bletch", "gronk"), ("1", "2")]
    , NewReq { name = "Fred"
             , isPublic = False
             , restoreState = Nothing }
    , NewReq { name = "Bob"
             , isPublic = True
             , restoreState = Nothing
             }
    , NewReq { name = "Bob"
             , isPublic = False
             , restoreState = Just <| Board.initialGameState False
             }
    , NewRsp { gameid = "asdf"
             , name = "John"
             }
    , NewRsp { gameid = "asdf"
             , name = "George"
             }
    , JoinReq { gameid = "asdf", name = "bill" }
    , JoinRsp { gameid = "asdf"
              , player = WhitePlayer
              , name = "bill"
              }
    , UpdateRsp { gameid = "asdf"
                , gameState = Board.initialGameState True
                }
    , PlaceReq { gameid = "asdf"
               , piece = (White, HandPiece)
               , node = "A1"
               }
    , SelectActorReq { gameid = "asdf"
                     , node = "A1"
                     }
    , SelectSubjectReq { gameid = "asdf"
                       , node = "A1"
                       }
    , MoveReq { gameid = "asdf"
              , node = "A1"
              }
    , GamesReq
    , GamesRsp [ { gameid = "1"
                 , playerName = "Bill"
                 }
               , { gameid = "2"
                 , playerName = "Bob"
                 }
               ]
    , UndoReq { gameid = "asdf" }
    , ErrorRsp { request = "foo", text = "Malformed request." }
    , ChatReq { gameid = "asdf", player = BlackPlayer, text = "Hello, World!" }
    , ChatRsp { gameid = "asdf", player = BlackPlayer, text = "Hello, World!" }
    ]

expectString : String -> String -> Expectation
expectString sb was =
    Expect.equal sb was

boardTest : String -> String -> Test
boardTest encodedBoard name =
    test ("boardTest \"" ++ name ++ "\"")
        (\_ ->
             let board = stringToBoard encodedBoard
             in
                 expectString encodedBoard <| boardToString board
        )

boardData : List String
boardData =
    [ "--hG4-wW7-tS5-O9-9-6-"
    , "-cs--mg"
    , "HC--TM-"
    , "--H9-3-Ww-S5-O--C--H6-m-G4-T4-"
    , "GM9---"
    , "--H9-3-Ww-S5-O--C--H6-m-G4-t3-M"
    , "G9-3-"
    , "tsc9--"
    , "--H5-w6-W--S5-O--C--H6-m-G4-t3-m"
    ]

gameStateTest : String -> String -> Test
gameStateTest encodedGameState name =
    test ("gameStateTest \"" ++ name ++ "\"")
        (\_ ->
             case ED.decodeGameState encodedGameState of
                 Err msg ->
                     expectString "" msg
                 Ok gs ->
                     expectString encodedGameState <| ED.encodeGameState gs
        )

gameStateData : List String
gameStateData =
    [ "[\"WP\",\"CA\",false,null,null,[\"--H9-3-Ww-S5-O--C--H6-m-G4-T4-\",\"GM9---\",\"tsc9--\"],[],[[\"WP\",\"CA\",false,[\"G3\",6,2,[\"W\",\"T\"]],[\"G7\",6,6,[\"B\",\"M\"]],[\"--H9-3-Ww-S5-O--C--H6-m-G4-t3-M\",\"G9-3-\",\"tsc9--\"],[\"--H5-w6-W--S5-O--C--H6-m-G4-t3-m\"],[[\"WP\",\"CA\",true,[\"G7\",6,6,[\"W\",\"M\"]],[\"B2\",1,1,[\"W\",\"W\"]],[\"--H5-w6-W--S5-O--C--H6-m-G4-t3-m\",\"G9-3-\",\"tsc9--\"],[\"--H5-w6-W--S5-O--C--H6-m-G4-t3-m\"],[]]]],[\"WP\",\"CA\",true,[\"G7\",6,6,[\"W\",\"M\"]],[\"B2\",1,1,[\"W\",\"W\"]],[\"--H5-w6-W--S5-O--C--H6-m-G4-t3-m\",\"G9-3-\",\"tsc9--\"],[\"--H5-w6-W--S5-O--C--H6-m-G4-t3-m\"],[]]]]"
    , "[\"WP\",\"S\",true,null,null,[\"--hG4-wW7-tS5-O9-9-6-\",\"-cs--mg\",\"HC--TM-\"],[],[]]"
    ]
