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
import Telegram.Methods as Telegram
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



-- INIT


type Model botModel
    = Initializing
    | Errored
    | Running (RunningModel botModel)


type alias RunningModel botModel =
    { token : Telegram.Token
    , botModel : botModel
    }


init : ConsolePort (Msg botMsg) -> Encode.Value -> ( Model model, Cmd (Msg botMsg) )
init consolePort flags =
    let
        tokenResult =
            Decode.decodeValue (Decode.field "token" Telegram.decodeToken) flags
    in
    case tokenResult of
        Ok token ->
            ( Initializing, Cmd.batch [ log consolePort "Deleting potential webhook.", Telegram.deleteWebhook token (DeletedWebhook token) ] )

        Err error ->
            ( Errored, logError consolePort <| Decode.errorToString error )


initModel :
    ConsolePort (Msg botMsg)
    -> Bot.Init model botMsg
    -> Telegram.Token
    -> Telegram.User
    -> ( Model model, Cmd (Msg botMsg) )
initModel consolePort botInit token self =
    ( Running
        { botModel = botInit self |> .model
        , token = token
        }
    , Cmd.batch
        [ Telegram.getUpdates token
            0
            processUpdates
        , log consolePort ("Bot '" ++ Elmegram.getDisplayName self ++ "' running.")
        ]
    )


processUpdates : Result Http.Error (List Telegram.Update) -> Msg botMsg
processUpdates result =
    case result of
        Ok updates ->
            NewUpdates updates

        Err error ->
            InvalidUpdate (httpErrorToString error)



-- UPDATE


type Msg botMsg
    = DeletedWebhook Telegram.Token (Result String ())
    | Init Telegram.Token (Result Http.Error Telegram.User)
    | NewUpdates (List Telegram.Update)
    | InvalidUpdate String
    | BotMsg botMsg
    | SentMethod Bot.Method (Result String ())


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
                    ( Initializing, Telegram.getMe token (Init token) )

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
                        |> Runner.updateFromStep
                            model.token
                            { log = outputToConsole consolePort
                            , botMsg = BotMsg
                            , methodSent = SentMethod
                            }
                            model.botModel
                        |> Tuple.mapFirst
                            (\botModel -> Running <| { model | botModel = botModel })
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
                                    , Telegram.getUpdates model.token offset processUpdates
                                    ]
                            )

                _ ->
                    ( runnerModel, logError consolePort "Got bot message even though bot initialization was unsuccessful." )

        BotMsg botMsg ->
            case runnerModel of
                Running model ->
                    Runner.update bot botMsg model.botModel
                        |> Runner.updateFromStep
                            model.token
                            { log = outputToConsole consolePort
                            , botMsg = BotMsg
                            , methodSent = SentMethod
                            }
                            model.botModel
                        |> Tuple.mapFirst
                            (\botModel -> Running <| { model | botModel = botModel })

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
