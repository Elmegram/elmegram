module Elmegram.Runner exposing
    ( ConsolePort
    , botRunner
    )

import Elmegram
import Elmegram.Bot as Bot exposing (Bot)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra as List
import Telegram
import Url exposing (Url)



-- INTERFACE


type alias ConsolePort msg =
    { level : String, message : String } -> Cmd msg


log : ConsolePort msg -> String -> Cmd msg
log console message =
    console { level = "log", message = message }


logError : ConsolePort msg -> String -> Cmd msg
logError console message =
    console { level = "error", message = message }


botRunner :
    Bot model msg
    -> ConsolePort (Msg msg)
    -> Platform.Program Encode.Value (Model model) (Msg msg)
botRunner bot consolePort =
    Platform.worker
        { init = init consolePort
        , update = update bot consolePort
        , subscriptions = \_ -> Sub.none
        }



-- INIT


type alias Token =
    String


type alias Model botModel =
    Maybe (RunningModel botModel)


type alias RunningModel botModel =
    { token : Token
    , botModel : botModel
    }


init : ConsolePort (Msg botMsg) -> Encode.Value -> ( Model model, Cmd (Msg botMsg) )
init consolePort flags =
    let
        tokenResult =
            Decode.decodeValue (Decode.field "token" Decode.string) flags
    in
    case tokenResult of
        Ok token ->
            ( Nothing, getMe token (Init token) )

        Err error ->
            ( Nothing, logError consolePort <| Decode.errorToString error )


initModel : ConsolePort (Msg botMsg) -> Bot.BotInit model botMsg -> Token -> Telegram.User -> ( Model model, Cmd (Msg botMsg) )
initModel consolePort botInit token self =
    ( Just
        { botModel = botInit self |> .model
        , token = token
        }
    , Cmd.batch
        [ getUpdates token 0
        , log consolePort ("Bot '" ++ Elmegram.getDisplayName self ++ "' running.")
        ]
    )



-- TELEGRAM API


getMe : Token -> (Result Http.Error Telegram.User -> msg) -> Cmd msg
getMe token tagger =
    Http.get
        { url = getMeUrl token |> Url.toString
        , expect = Http.expectJson tagger (Decode.field "result" Telegram.decodeUser)
        }


getMeUrl : Token -> Url
getMeUrl token =
    getMethodUrl token "getMe"


getUpdates : Token -> Int -> Cmd (Msg msg)
getUpdates token offset =
    Http.post
        { url = getUpdatesUrl token |> Url.toString
        , expect =
            Http.expectJson
                (\result ->
                    case result of
                        Ok updates ->
                            NewUpdate updates

                        Err error ->
                            InvalidUpdate (httpErrorToString error)
                )
                (Decode.field "result" <| Decode.list Telegram.decodeUpdate)
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "offset", Encode.int offset )
                    ]
                )
        }


getUpdatesUrl : Token -> Url
getUpdatesUrl token =
    getMethodUrl token "getUpdates"


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



-- UPDATE


type Msg botMsg
    = Init Token (Result Http.Error Telegram.User)
    | NewUpdate (List Telegram.Update)
    | InvalidUpdate String
    | BotMsg botMsg
    | SentMethod Bot.Method (Result String String)


update :
    Bot model botMsg
    -> ConsolePort (Msg botMsg)
    -> Msg botMsg
    -> Model model
    -> ( Model model, Cmd (Msg botMsg) )
update bot consolePort msg maybeModel =
    case msg of
        Init token getMeResult ->
            case getMeResult of
                Ok self ->
                    initModel consolePort bot.init token self

                Err error ->
                    ( Nothing, logError consolePort ("Error while initializing bot:\n" ++ httpErrorToString error) )

        NewUpdate updates ->
            case maybeModel of
                Just model ->
                    processUpdates model.token bot model consolePort updates
                        |> Tuple.mapFirst Just
                        |> Tuple.mapSecond
                            (\cmd ->
                                Cmd.batch
                                    [ log consolePort
                                        ("Received updates:\n"
                                            ++ Encode.encode 2 (Encode.list Telegram.encodeUpdate updates)
                                        )
                                    , cmd
                                    ]
                            )

                Nothing ->
                    ( Nothing, logError consolePort "Got new update even though bot initialization was unsuccessful." )

        InvalidUpdate error ->
            ( maybeModel, logError consolePort ("Error while getting update:\n" ++ error) )

        BotMsg botMsg ->
            case maybeModel of
                Just model ->
                    bot.update botMsg model.botModel
                        |> updateFromResponse consolePort model.token
                        |> Tuple.mapFirst (\botModel -> Just { model | botModel = botModel })

                Nothing ->
                    ( Nothing, logError consolePort "Got bot message even though bot initialization was unsuccessful." )

        SentMethod method result ->
            case result of
                Ok _ ->
                    ( maybeModel
                    , Cmd.none
                    )

                Err error ->
                    ( Nothing
                    , let
                        { methodName, content } =
                            Bot.encodeMethod method
                      in
                      logError consolePort
                        ("Sending of method was unsuccessful. Reason: "
                            ++ error
                            ++ ".\nMethod was '"
                            ++ methodName
                            ++ "' containing:\n"
                            ++ Encode.encode 2 content
                        )
                    )


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl url ->
            "Requested url '" ++ url ++ "' is invalid."

        Http.Timeout ->
            "Timed out."

        Http.NetworkError ->
            "Network error."

        Http.BadStatus status ->
            "Response had bad status " ++ String.fromInt status ++ "."

        Http.BadBody bodyError ->
            "Response body had an issue:\n" ++ bodyError


processUpdates :
    Token
    -> Bot botModel botMsg
    -> RunningModel botModel
    -> ConsolePort (Msg botMsg)
    -> List Telegram.Update
    -> ( RunningModel botModel, Cmd (Msg botMsg) )
processUpdates token bot model consolePort updates =
    List.foldl
        (\newUpdate ( previousModel, previousCmd ) ->
            processUpdate bot.newUpdateMsg bot.update previousModel newUpdate
                |> updateFromResponse consolePort token
        )
        ( model.botModel, Cmd.none )
        updates
        |> Tuple.mapFirst (\botModel -> { model | botModel = botModel })
        |> (let
                offset =
                    case List.last updates of
                        Just lastUpdate ->
                            Telegram.getNextOffset lastUpdate.update_id

                        Nothing ->
                            0
            in
            Tuple.mapSecond (\cmd -> Cmd.batch [ cmd, getUpdates token offset ])
           )


processUpdate :
    Bot.BotNewUpdateMsg botMsg
    -> Bot.BotUpdate botModel botMsg
    -> botModel
    -> Telegram.Update
    -> Bot.Response botModel botMsg
processUpdate botNewUpdateMsg botUpdate model newUpdate =
    botUpdate (botNewUpdateMsg newUpdate) model


updateFromResponse : ConsolePort (Msg botMsg) -> Token -> Bot.Response model botMsg -> ( model, Cmd (Msg botMsg) )
updateFromResponse consolePort token response =
    ( response.model, cmdFromResponse consolePort token response )


cmdFromResponse : ConsolePort (Msg botMsg) -> Token -> Bot.Response model botMsg -> Cmd (Msg botMsg)
cmdFromResponse consolePort token response =
    Cmd.batch
        ([ Cmd.map BotMsg response.command
         ]
            ++ List.map (sendMethod consolePort token) response.methods
        )


sendMethod : ConsolePort (Msg botMsg) -> Token -> Bot.Method -> Cmd (Msg botMsg)
sendMethod consolePort token method =
    let
        { methodName, content } =
            Bot.encodeMethod method

        parseSendMethodResponse response =
            case response of
                Http.BadUrl_ url ->
                    Err ("Url was invalid: " ++ url)

                Http.Timeout_ ->
                    Err "Timeout."

                Http.NetworkError_ ->
                    Err "NetworkError"

                Http.BadStatus_ { statusCode } body ->
                    case Decode.decodeString (Decode.field "description" Decode.string) body of
                        Ok description ->
                            Err description

                        Err err ->
                            Err (Decode.errorToString err)

                Http.GoodStatus_ { statusCode } body ->
                    Ok body
    in
    Cmd.batch
        [ log consolePort ("Called " ++ methodName ++ " with:\n" ++ Encode.encode 2 content)
        , Http.post
            { url = getMethodUrl token methodName |> Url.toString
            , body = Http.jsonBody content
            , expect = Http.expectStringResponse (SentMethod method) parseSendMethodResponse
            }
        ]
