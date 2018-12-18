module Elmegram.Runner exposing (BotInit, BotUpdate, ErrorPort, IncomingUpdatePort, MethodPort, botRunner)

import Elmegram exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import Telegram


type alias Bot model msg =
    { init : BotInit model
    , newUpdateMsg : BotNewUpdateMsg msg
    , update : BotUpdate model msg
    }


type alias BotInit model =
    Telegram.User -> model


type alias BotNewUpdateMsg msg =
    Telegram.Update -> msg


type alias BotUpdate model msg =
    msg -> model -> Response model msg


type alias IncomingUpdatePort msg =
    (Encode.Value -> msg) -> Sub msg


type alias MethodPort msg =
    Encode.Value -> Cmd msg


type alias ErrorPort msg =
    String -> Cmd msg


botRunner :
    Bot model msg
    ->
        { incomingUpdatePort : IncomingUpdatePort (Msg msg)
        , methodPort : MethodPort (Msg msg)
        , errorPort : ErrorPort (Msg msg)
        }
    -> Platform.Program Encode.Value (Model model) (Msg msg)
botRunner bot ports =
    Platform.worker
        { init = init bot.init ports.errorPort
        , update = update bot.newUpdateMsg bot.update ports.methodPort ports.errorPort
        , subscriptions = subscriptions ports.incomingUpdatePort
        }


type alias Model botModel =
    Maybe botModel


init : BotInit model -> ErrorPort (Msg botMsg) -> Encode.Value -> ( Model model, Cmd (Msg botMsg) )
init botInit errorPort flags =
    let
        selfResult =
            Decode.decodeValue Telegram.decodeUser flags
    in
    case selfResult of
        Ok self ->
            ( Just <| botInit self, Cmd.none )

        Err error ->
            ( Nothing, errorPort <| Decode.errorToString error )


type Msg botMsg
    = NewUpdate UpdateResult
    | BotMsg botMsg


update : BotNewUpdateMsg botMsg -> BotUpdate model botMsg -> MethodPort (Msg botMsg) -> ErrorPort (Msg botMsg) -> Msg botMsg -> Model model -> ( Model model, Cmd (Msg botMsg) )
update botNewUpdateMsg botUpdate methodPort errorPort msg maybeModel =
    case maybeModel of
        Just model ->
            case msg of
                NewUpdate result ->
                    processUpdate botNewUpdateMsg botUpdate methodPort errorPort result model
                        |> Tuple.mapFirst Just

                BotMsg botMsg ->
                    botUpdate botMsg model
                        |> updateFromResponse methodPort
                        |> Tuple.mapFirst Just

        Nothing ->
            ( Nothing, Cmd.none )


type alias UpdateResult =
    Result Decode.Error Telegram.Update


processUpdate : BotNewUpdateMsg botMsg -> BotUpdate model botMsg -> MethodPort (Msg botMsg) -> ErrorPort (Msg botMsg) -> UpdateResult -> model -> ( model, Cmd (Msg botMsg) )
processUpdate botNewUpdateMsg botUpdate methodPort errorPort result model =
    case result of
        Err err ->
            ( model, Decode.errorToString err |> errorPort )

        Ok newUpdate ->
            botUpdate (botNewUpdateMsg newUpdate) model
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
