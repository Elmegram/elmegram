module Elmegram.Runner exposing (BotInit, BotUpdate, ErrorPort, IncomingUpdatePort, MethodPort, botRunner)

import Elmegram exposing (..)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Telegram
import Url exposing (Url)


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
        { init = init ports.errorPort
        , update = update bot.init bot.newUpdateMsg bot.update ports.methodPort ports.errorPort
        , subscriptions = subscriptions ports.incomingUpdatePort
        }


type alias Token =
    String


type alias Model botModel =
    Maybe
        { botModel : botModel
        , token : Token
        }


init : ErrorPort (Msg botMsg) -> Encode.Value -> ( Model model, Cmd (Msg botMsg) )
init errorPort flags =
    let
        tokenResult =
            Decode.decodeValue (Decode.field "token" Decode.string) flags
    in
    case tokenResult of
        Ok token ->
            let
                getMe =
                    Http.get
                        { url = getMeUrl token |> Url.toString
                        , expect = Http.expectJson (Init token) (Decode.field "result" Telegram.decodeUser)
                        }
            in
            ( Nothing, getMe )

        Err error ->
            ( Nothing, errorPort <| Decode.errorToString error )


getMeUrl : Token -> Url
getMeUrl token =
    getMethodUrl token "getMe"


getMethodUrl : Token -> String -> Url
getMethodUrl token method =
    let
        baseUrl =
            getBaseUrl token
    in
    { baseUrl | path = baseUrl.path ++ "/" ++ method }


getBaseUrl : String -> Url
getBaseUrl token =
    { protocol = Url.Https
    , host = "api.telegram.org"
    , port_ = Nothing
    , path = "/bot" ++ token
    , query = Nothing
    , fragment = Nothing
    }


type Msg botMsg
    = Init Token (Result Http.Error Telegram.User)
    | NewUpdate UpdateResult
    | BotMsg botMsg


update :
    BotInit model
    -> BotNewUpdateMsg botMsg
    -> BotUpdate model botMsg
    -> MethodPort (Msg botMsg)
    -> ErrorPort (Msg botMsg)
    -> Msg botMsg
    -> Model model
    -> ( Model model, Cmd (Msg botMsg) )
update botInit botNewUpdateMsg botUpdate methodPort errorPort msg maybeModel =
    case msg of
        Init token getMeResult ->
            case getMeResult of
                Ok self ->
                    ( Just
                        { botModel = botInit self
                        , token = token
                        }
                    , Cmd.none
                    )

                Err error ->
                    case error of
                        Http.BadBody errorMsg ->
                            ( Nothing, errorPort ("Error while initializing bot:\n" ++ errorMsg) )

                        Http.BadStatus status ->
                            ( Nothing, errorPort ("Error while initializing bot:\nResponse had status " ++ String.fromInt status ++ ".") )

                        Http.NetworkError ->
                            ( Nothing, errorPort "Network error while initializing bot." )

                        Http.Timeout ->
                            ( Nothing, errorPort "Timeout while initializing bot." )

                        Http.BadUrl url ->
                            ( Nothing, errorPort ("Bad url while initializing bot:\nUrl was " ++ url ++ ".") )

        NewUpdate result ->
            case maybeModel of
                Just model ->
                    processUpdate botNewUpdateMsg botUpdate methodPort errorPort result model.botModel
                        |> Tuple.mapFirst (\botModel -> Just { model | botModel = botModel })

                Nothing ->
                    ( Nothing, errorPort "Got new update even though bot initialization was unsuccessful." )

        BotMsg botMsg ->
            case maybeModel of
                Just model ->
                    botUpdate botMsg model.botModel
                        |> updateFromResponse methodPort
                        |> Tuple.mapFirst (\botModel -> Just { model | botModel = botModel })

                Nothing ->
                    ( Nothing, errorPort "Got bot message even though bot initialization was unsuccessful." )


type alias UpdateResult =
    Result Decode.Error Telegram.Update


processUpdate :
    BotNewUpdateMsg botMsg
    -> BotUpdate model botMsg
    -> MethodPort (Msg botMsg)
    -> ErrorPort (Msg botMsg)
    -> UpdateResult
    -> model
    -> ( model, Cmd (Msg botMsg) )
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
