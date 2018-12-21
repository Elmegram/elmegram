module Elmegram.Runner exposing (Log, LogLevel(..), Step, newUpdates, update, updateFromStep)

import Elmegram.Bot as Bot
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Telegram
import Telegram.Methods as Telegram


newUpdates :
    { a
        | newUpdateMsg : Bot.NewUpdateMsg botMsg
        , update : Bot.Update botModel botMsg
    }
    -> List Telegram.Update
    -> botModel
    -> Step botModel botMsg
newUpdates bot updates botModel =
    List.foldl
        (\newUpdate ( previousMethods, previousModel, previousCmd ) ->
            let
                response =
                    bot.update (bot.newUpdateMsg newUpdate) botModel
            in
            ( previousMethods ++ response.methods
            , response.model
            , Cmd.batch [ previousCmd, response.cmd ]
            )
        )
        ( [], botModel, Cmd.none )
        updates
        |> (\( methods, model, cmd ) ->
                { model = model
                , cmd = cmd
                , methods = methods
                , logs =
                    [ Log Info
                        ("Received updates:\n"
                            ++ Encode.encode 2 (Encode.list Telegram.encodeUpdate updates)
                        )
                    ]
                }
           )


update :
    { a
        | newUpdateMsg : Bot.NewUpdateMsg botMsg
        , update : Bot.Update botModel botMsg
    }
    -> botMsg
    -> botModel
    -> Step botModel botMsg
update bot botMsg botModel =
    bot.update botMsg botModel
        |> stepFromResponse


type alias Step botModel botMsg =
    { model : botModel
    , cmd : Cmd botMsg
    , methods : List Bot.Method
    , logs : List Log
    }


stepFromResponse : Bot.Response botModel botMsg -> Step botModel botMsg
stepFromResponse response =
    { model = response.model
    , cmd = response.cmd
    , methods = response.methods
    , logs = []
    }


updateFromStep :
    Telegram.Token
    ->
        { log : Log -> Cmd msg
        , botMsg : botMsg -> msg
        , methodSent : Bot.Method -> Result String () -> msg
        }
    -> botModel
    -> Step botModel botMsg
    -> ( botModel, Cmd msg )
updateFromStep token { log, botMsg, methodSent } model step =
    let
        cmd =
            Cmd.batch
                (List.map log step.logs
                    ++ List.map (sendMethod log methodSent token) step.methods
                    ++ [ Cmd.map botMsg step.cmd
                       ]
                )
    in
    ( model, cmd )


sendMethod : (Log -> Cmd msg) -> (Bot.Method -> Result String () -> msg) -> Telegram.Token -> Bot.Method -> Cmd msg
sendMethod log methodTagger token method =
    let
        { methodName, content } =
            Bot.encodeMethod method
    in
    Cmd.batch
        [ log (Log Info ("Called " ++ methodName ++ " with:\n" ++ Encode.encode 2 content))
        , Bot.sendMethod token method (methodTagger method)
        ]



-- LOG


type alias Log =
    { level : LogLevel, message : String }


type LogLevel
    = Info
    | Error
