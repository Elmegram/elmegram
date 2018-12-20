module Elmegram.Polling exposing
    ( ConsolePort
    , botRunner
    )

import Elmegram
import Elmegram.Bot as Bot exposing (Bot)
import Elmegram.Runner as Runner
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra as List
import Telegram
import Url exposing (Url)


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



-- CONSOLE


type alias ConsolePort msg =
    { level : String, message : String } -> Cmd msg


log : ConsolePort msg -> String -> Cmd msg
log consolePort message =
    outputToConsole consolePort <| Runner.Log Runner.Info message


logError : ConsolePort msg -> String -> Cmd msg
logError consolePort message =
    outputToConsole consolePort <| Runner.Log Runner.Error message


outputToConsole : ConsolePort msg -> Runner.Log -> Cmd msg
outputToConsole consolePort l =
    let
        levelString =
            case l.level of
                Runner.Info ->
                    "log"

                Runner.Error ->
                    "error"
    in
    consolePort { level = levelString, message = l.message }



-- INIT


type Token
    = Token String


stringFromToken : Token -> String
stringFromToken (Token token) =
    token


decodeToken : Decode.Decoder Token
decodeToken =
    Decode.string |> Decode.map Token


type Model botModel
    = Initializing
    | Errored
    | Running (RunningModel botModel)


type alias RunningModel botModel =
    { token : Token
    , botModel : botModel
    }


init : ConsolePort (Msg botMsg) -> Encode.Value -> ( Model model, Cmd (Msg botMsg) )
init consolePort flags =
    let
        tokenResult =
            Decode.decodeValue (Decode.field "token" decodeToken) flags
    in
    case tokenResult of
        Ok token ->
            ( Initializing, Cmd.batch [ log consolePort "Deleting potential webhook.", deleteWebhook token (DeletedWebhook token) ] )

        Err error ->
            ( Errored, logError consolePort <| Decode.errorToString error )


initModel :
    ConsolePort (Msg botMsg)
    -> Bot.Init model botMsg
    -> Token
    -> Telegram.User
    -> ( Model model, Cmd (Msg botMsg) )
initModel consolePort botInit token self =
    ( Running
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
getMeUrl =
    getMethodUrl "getMe"


deleteWebhook : Token -> (Result String () -> msg) -> Cmd msg
deleteWebhook token tagger =
    Http.post
        { url = deleteWebhookUrl token |> Url.toString
        , expect =
            Http.expectJson
                (\result ->
                    Result.mapError httpErrorToString result
                        |> Result.andThen
                            (\response ->
                                if response.ok then
                                    Ok ()

                                else
                                    case response.description of
                                        Just description ->
                                            Err ("Response JSON indicated error:\n" ++ description)

                                        Nothing ->
                                            Err "Response JSON indicated error, but no description was given."
                            )
                        |> tagger
                )
                (Decode.map2 (\ok description -> { ok = ok, description = description })
                    (Decode.field "ok" Decode.bool)
                    (Decode.maybe (Decode.field "description" Decode.string))
                )
        , body = Http.emptyBody
        }


deleteWebhookUrl : Token -> Url
deleteWebhookUrl =
    getMethodUrl "deleteWebhook"


getUpdates : Token -> Int -> Cmd (Msg msg)
getUpdates token offset =
    Http.post
        { url = getUpdatesUrl token |> Url.toString
        , expect =
            Http.expectJson
                (\result ->
                    case result of
                        Ok updates ->
                            NewUpdates updates

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
getUpdatesUrl =
    getMethodUrl "getUpdates"


getMethodUrl : String -> Token -> Url
getMethodUrl method token =
    let
        baseUrl =
            getBaseUrl token
    in
    { baseUrl | path = baseUrl.path ++ "/" ++ method }


getBaseUrl : Token -> Url
getBaseUrl token =
    { protocol = Url.Https
    , host = "api.telegram.org"
    , port_ = Nothing
    , path = "/bot" ++ stringFromToken token
    , query = Nothing
    , fragment = Nothing
    }



-- UPDATE


type Msg botMsg
    = DeletedWebhook Token (Result String ())
    | Init Token (Result Http.Error Telegram.User)
    | NewUpdates (List Telegram.Update)
    | InvalidUpdate String
    | BotMsg botMsg
    | SentMethod Bot.Method (Result String String)


update :
    Bot model botMsg
    -> ConsolePort (Msg botMsg)
    -> Msg botMsg
    -> Model model
    -> ( Model model, Cmd (Msg botMsg) )
update bot consolePort msg runnerModel =
    case msg of
        DeletedWebhook token result ->
            case result of
                Ok _ ->
                    ( Initializing, getMe token (Init token) )

                Err error ->
                    ( Errored, logError consolePort error )

        Init token getMeResult ->
            case getMeResult of
                Ok self ->
                    initModel consolePort bot.init token self

                Err error ->
                    ( Errored, logError consolePort ("Error while initializing bot:\n" ++ httpErrorToString error) )

        InvalidUpdate error ->
            ( runnerModel, logError consolePort ("Error while getting update:\n" ++ error) )

        NewUpdates updates ->
            case runnerModel of
                Running model ->
                    Runner.newUpdates bot updates model.botModel
                        |> updateFromStep consolePort model
                        |> Tuple.mapSecond
                            (\cmd ->
                                let
                                    offset =
                                        case List.last updates of
                                            Just lastUpdate ->
                                                Telegram.getNextOffset lastUpdate.update_id

                                            Nothing ->
                                                0
                                in
                                Cmd.batch
                                    [ cmd
                                    , getUpdates model.token offset
                                    ]
                            )

                _ ->
                    ( runnerModel, logError consolePort "Got bot message even though bot initialization was unsuccessful." )

        BotMsg botMsg ->
            case runnerModel of
                Running model ->
                    Runner.update bot botMsg model.botModel
                        |> updateFromStep consolePort model

                _ ->
                    ( runnerModel, logError consolePort "Got bot message even though bot initialization was unsuccessful." )

        SentMethod method result ->
            case result of
                Ok _ ->
                    ( runnerModel
                    , Cmd.none
                    )

                Err error ->
                    ( runnerModel
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


updateFromStep :
    ConsolePort (Msg botMsg)
    -> RunningModel botModel
    -> Runner.Step botModel botMsg
    -> ( Model botModel, Cmd (Msg botMsg) )
updateFromStep consolePort model step =
    let
        newModel =
            { model | botModel = step.model }

        cmd =
            Cmd.batch
                (List.map (outputToConsole consolePort) step.logs
                    ++ List.map (sendMethod consolePort model.token) step.methods
                    ++ [ Cmd.map BotMsg step.cmd
                       ]
                )
    in
    ( Running newModel, cmd )


sendMethod : ConsolePort (Msg botMsg) -> Token -> Bot.Method -> Cmd (Msg botMsg)
sendMethod consolePort token method =
    let
        { methodName, content } =
            Bot.encodeMethod method

        parseSendMethodResponse response =
            case response of
                Http.BadUrl_ url ->
                    Err <| httpErrorToString (Http.BadUrl url)

                Http.Timeout_ ->
                    Err <| httpErrorToString Http.Timeout

                Http.NetworkError_ ->
                    Err <| httpErrorToString Http.NetworkError

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
            { url = getMethodUrl methodName token |> Url.toString
            , body = Http.jsonBody content
            , expect = Http.expectStringResponse (SentMethod method) parseSendMethodResponse
            }
        ]
