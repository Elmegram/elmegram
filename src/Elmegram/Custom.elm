module Elmegram.Custom exposing
    ( ConsolePort
    , IncomingUpdatePort
    , SendMethodPort
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
    Bot botModel botMsg
    ->
        { console : ConsolePort (Msg botMsg)
        , incomingUpdate : IncomingUpdatePort (Msg botMsg)
        , sendMethod : SendMethodPort (Msg botMsg)
        }
    -> Platform.Program Encode.Value (Model botModel) (Msg botMsg)
botRunner bot ports =
    Platform.worker
        { init = init ports.console
        , update = update bot ports
        , subscriptions = subscriptions ports.incomingUpdate
        }


subscriptions : IncomingUpdatePort (Msg botMsg) -> a -> Sub (Msg botMsg)
subscriptions incomingUpdatePort _ =
    incomingUpdatePort
        (\raw ->
            case Decode.decodeValue (Decode.list Telegram.decodeUpdate) raw of
                Ok updates ->
                    NewUpdates updates

                Err error ->
                    InvalidUpdate (Decode.errorToString error)
        )



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
            ( Initializing, Telegram.getMe token (Init token) )

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
        [ log consolePort ("Bot '" ++ Elmegram.getDisplayName self ++ "' running.")
        ]
    )



-- UPDATE


type Msg botMsg
    = Init Telegram.Token (Result Http.Error Telegram.User)
    | NewUpdates (List Telegram.Update)
    | InvalidUpdate String
    | BotMsg botMsg


update :
    Bot model botMsg
    ->
        { a
            | console : ConsolePort (Msg botMsg)
            , sendMethod : SendMethodPort (Msg botMsg)
        }
    -> Msg botMsg
    -> Model model
    -> ( Model model, Cmd (Msg botMsg) )
update bot ports msg runnerModel =
    case msg of
        Init token getMeResult ->
            case getMeResult of
                Ok self ->
                    initModel ports.console bot.init token self

                Err error ->
                    ( Errored, logError ports.console ("Error while initializing bot:\n" ++ httpErrorToString error) )

        InvalidUpdate error ->
            ( runnerModel, logError ports.console ("Error while getting update:\n" ++ error) )

        NewUpdates updates ->
            case runnerModel of
                Running model ->
                    Runner.newUpdates bot updates model.botModel
                        |> updateFromStep model.token ports
                        |> Tuple.mapFirst (\botModel -> Running <| { model | botModel = botModel })

                _ ->
                    ( runnerModel, logError ports.console "Got bot message even though bot initialization was unsuccessful." )

        BotMsg botMsg ->
            case runnerModel of
                Running model ->
                    Runner.update bot botMsg model.botModel
                        |> updateFromStep model.token ports
                        |> Tuple.mapFirst (\botModel -> Running <| { model | botModel = botModel })

                _ ->
                    ( runnerModel, logError ports.console "Got bot message even though bot initialization was unsuccessful." )


updateFromStep :
    Telegram.Token
    ->
        { a
            | console : ConsolePort (Msg botMsg)
            , sendMethod : SendMethodPort (Msg botMsg)
        }
    -> Runner.Step botModel botMsg
    -> ( botModel, Cmd (Msg botMsg) )
updateFromStep token ports step =
    let
        sendLogsCmd =
            Cmd.batch
                (List.map (outputToConsole ports.console) step.logs)

        methodToValue method =
            Encode.object
                [ ( "methodName", Encode.string method.methodName )
                , ( "content", method.content )
                ]

        sendMethodsCmd =
            Cmd.batch
                (List.map
                    (Bot.encodeMethod >> methodToValue >> ports.sendMethod)
                    step.methods
                )

        botCmd =
            Cmd.map BotMsg step.cmd
    in
    ( step.model
    , Cmd.batch
        [ sendMethodsCmd, botCmd, sendLogsCmd ]
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



--PORTS


type alias ConsolePort msg =
    { level : String, message : String } -> Cmd msg


type alias IncomingUpdatePort msg =
    (Encode.Value -> msg) -> Sub msg


type alias SendMethodPort msg =
    Encode.Value -> Cmd msg



-- CONSOLE


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
