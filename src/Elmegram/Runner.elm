module Elmegram.Runner exposing (Log, LogLevel(..), Step, newUpdates, update)

import Elmegram.Bot as Bot
import Json.Encode as Encode
import Telegram


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



-- LOG


type alias Log =
    { level : LogLevel, message : String }


type LogLevel
    = Info
    | Error
