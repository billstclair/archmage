----------------------------------------------------------------------
--
-- ArchmagePorts.elm
-- Top level user interface for Chris St. Clair's Archmage board game.
-- Archmage.elm is the same thing without saved state, but runnable in elm-reactor
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

port module ArchmagePorts exposing (..)

import Archmage exposing ( init, view, update, subscriptions )
import Archmage.Types as Types exposing ( Model, Msg )
import Archmage.Server.EncodeDecode exposing ( encodeModel, decodeModel )

import Html
import Debug exposing ( log )

port setStorage : String -> Cmd a

main : Program (Maybe String) Model Msg
main =
    Html.programWithFlags
        { init = initWithStorage
        , view = view
        , update = updateWithStorage
        , subscriptions = subscriptions
        }

-- Copied verbatim from https://github.com/evancz/elm-todomvc/blob/master/Todo.elm
updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let ( newModel, cmds ) = update msg model
        json = encodeModel newModel
    in
        ( newModel
        , Cmd.batch [ setStorage json
                    , cmds
                    ]
        )

initWithStorage : Maybe String -> ( Model, Cmd Msg )
initWithStorage maybeJson =
    case maybeJson of
        Nothing ->
            init Nothing
        Just json ->
            case decodeModel json of
                Err _ ->
                    init Nothing
                Ok model ->
                    init <| Just model
