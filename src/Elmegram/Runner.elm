module Elmegram.Runner exposing (BotHandle, BotInit, BotUpdate, ErrorPort, IncomingUpdatePort, MethodPort, bot)

import Elmegram exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import Telegram


type alias BotInit model =
    Telegram.User -> model


type alias BotHandle model msg =
    Telegram.Update -> model -> Response model msg


type alias BotUpdate model msg =
    msg -> model -> Response model msg


type alias IncomingUpdatePort msg =
    (Encode.Value -> msg) -> Sub msg


type alias MethodPort msg =
    Encode.Value -> Cmd msg


type alias ErrorPort msg =
    String -> Cmd msg


bot :
    { init : BotInit model
    , handle : BotHandle model msg
    , update : BotUpdate model msg
    , incomingUpdatePort : IncomingUpdatePort (Msg msg)
    , methodPort : MethodPort (Msg msg)
    , errorPort : ErrorPort (Msg msg)
    }
    -> Platform.Program RawUser model (Msg msg)
bot config =
    Platform.worker
        { init = init config.init
        , update = update config.handle config.update config.methodPort config.errorPort
        , subscriptions = subscriptions config.incomingUpdatePort
        }


{-| Simple type without custom types. This can be used by init, forcing errors
on the JS side instead of in a Decoder.

The only difference to `Telegram.User` is the id field. The real user has
a phantom type that guards against mixing incompatible ids.

-}
type alias RawUser =
    { id : Int
    , is_bot : Bool
    , first_name : String
    , last_name : Maybe String
    , username : Maybe String
    , language_code : Maybe String
    }


init : BotInit model -> RawUser -> ( model, Cmd (Msg botMsg) )
init botInit rawBot =
    let
        self =
            -- Small hack to make type safe ID.
            { id = Telegram.makeTestId rawBot.id
            , is_bot = rawBot.is_bot
            , first_name = rawBot.first_name
            , last_name = rawBot.last_name
            , username = rawBot.username
            , language_code = rawBot.language_code
            }
    in
    ( botInit self, Cmd.none )


type Msg botMsg
    = NewUpdate UpdateResult
    | BotMsg botMsg


update : BotHandle model botMsg -> BotUpdate model botMsg -> MethodPort (Msg botMsg) -> ErrorPort (Msg botMsg) -> Msg botMsg -> model -> ( model, Cmd (Msg botMsg) )
update botHandle botUpdate methodPort errorPort msg model =
    case msg of
        NewUpdate result ->
            processUpdate botHandle methodPort errorPort result model

        BotMsg botMsg ->
            botUpdate botMsg model
                |> updateFromResponse methodPort


type alias UpdateResult =
    Result Decode.Error Telegram.Update


processUpdate : BotHandle model botMsg -> MethodPort (Msg botMsg) -> ErrorPort (Msg botMsg) -> UpdateResult -> model -> ( model, Cmd (Msg botMsg) )
processUpdate updateHandler methodPort errorPort result model =
    case result of
        Err err ->
            ( model, Decode.errorToString err |> errorPort )

        Ok newUpdate ->
            updateHandler newUpdate model
                |> updateFromResponse methodPort


updateFromResponse : MethodPort (Msg botMsg) -> Elmegram.Response model botMsg -> ( model, Cmd (Msg botMsg) )
updateFromResponse methodPort response =
    ( response.model, cmdFromResponse methodPort response )


cmdFromResponse : MethodPort (Msg botMsg) -> Elmegram.Response model botMsg -> Cmd (Msg botMsg)
cmdFromResponse methodPort response =
    Cmd.batch
        ([ Cmd.map BotMsg response.command
         ]
            ++ (if List.isEmpty response.methods then
                    []

                else
                    [ methodPort (Encode.list Elmegram.encodeMethod response.methods) ]
               )
        )


subscriptions : IncomingUpdatePort (Msg botMsg) -> model -> Sub (Msg botMsg)
subscriptions incomingUpdatePort model =
    incomingUpdatePort (Decode.decodeValue Telegram.decodeUpdate >> NewUpdate)
