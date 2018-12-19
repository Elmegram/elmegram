module Elmegram.Runner exposing
    ( ErrorPort
    , botRunner
    )

import Elmegram.Bot as Bot exposing (Bot)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra as List
import Telegram
import Url exposing (Url)



-- INTERFACE


type alias ErrorPort msg =
    String -> Cmd msg


botRunner :
    Bot model msg
    -> ErrorPort (Msg msg)
    -> Platform.Program Encode.Value (Model model) (Msg msg)
botRunner bot errorPort =
    Platform.worker
        { init = init errorPort
        , update = update bot errorPort
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


init : ErrorPort (Msg botMsg) -> Encode.Value -> ( Model model, Cmd (Msg botMsg) )
init errorPort flags =
    let
        tokenResult =
            Decode.decodeValue (Decode.field "token" Decode.string) flags
    in
    case tokenResult of
        Ok token ->
            ( Nothing, getMe token (Init token) )

        Err error ->
            ( Nothing, errorPort <| Decode.errorToString error )


initModel : Bot.BotInit model msg -> Token -> Telegram.User -> ( Model model, Cmd (Msg botMsg) )
initModel botInit token self =
    ( Just
        { botModel = botInit self |> .model
        , token = token
        }
    , getUpdates token 0 NewUpdate
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


getUpdates : Token -> Int -> (Result Http.Error (List Telegram.Update) -> msg) -> Cmd msg
getUpdates token offset tagger =
    Http.post
        { url = getUpdatesUrl token |> Url.toString
        , expect = Http.expectJson tagger (Decode.field "result" <| Decode.list Telegram.decodeUpdate)
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
    | NewUpdate (Result Http.Error (List Telegram.Update))
    | BotMsg botMsg
    | SentMethod Bot.Method (Result String String)


update :
    Bot model botMsg
    -> ErrorPort (Msg botMsg)
    -> Msg botMsg
    -> Model model
    -> ( Model model, Cmd (Msg botMsg) )
update bot errorPort msg maybeModel =
    case msg of
        Init token getMeResult ->
            case getMeResult of
                Ok self ->
                    initModel bot.init token self

                Err error ->
                    ( Nothing, errorPort ("Error while initializing bot:\n" ++ httpErrorToString error) )

        NewUpdate result ->
            case maybeModel of
                Just model ->
                    processUpdates model.token bot model errorPort result
                        |> Tuple.mapFirst Just

                Nothing ->
                    ( Nothing, errorPort "Got new update even though bot initialization was unsuccessful." )

        BotMsg botMsg ->
            case maybeModel of
                Just model ->
                    bot.update botMsg model.botModel
                        |> updateFromResponse model.token
                        |> Tuple.mapFirst (\botModel -> Just { model | botModel = botModel })

                Nothing ->
                    ( Nothing, errorPort "Got bot message even though bot initialization was unsuccessful." )

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
                      errorPort
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
    -> ErrorPort (Msg botMsg)
    -> Result Http.Error (List Telegram.Update)
    -> ( RunningModel botModel, Cmd (Msg botMsg) )
processUpdates token bot model errorPort updatesResult =
    case updatesResult of
        Ok updates ->
            List.foldl
                (\newUpdate ( previousModel, previousCmd ) ->
                    processUpdate bot.newUpdateMsg bot.update previousModel newUpdate
                        |> updateFromResponse token
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
                    Tuple.mapSecond (\cmd -> Cmd.batch [ cmd, getUpdates token offset NewUpdate ])
                   )

        Err error ->
            ( model, errorPort ("Error while processing new updates:\n" ++ httpErrorToString error) )


processUpdate :
    Bot.BotNewUpdateMsg botMsg
    -> Bot.BotUpdate botModel botMsg
    -> botModel
    -> Telegram.Update
    -> Bot.Response botModel botMsg
processUpdate botNewUpdateMsg botUpdate model newUpdate =
    botUpdate (botNewUpdateMsg newUpdate) model


updateFromResponse : Token -> Bot.Response model botMsg -> ( model, Cmd (Msg botMsg) )
updateFromResponse token response =
    ( response.model, cmdFromResponse token response )


cmdFromResponse : Token -> Bot.Response model botMsg -> Cmd (Msg botMsg)
cmdFromResponse token response =
    Cmd.batch
        ([ Cmd.map BotMsg response.command
         ]
            ++ List.map (sendMethod token) response.methods
        )


sendMethod : Token -> Bot.Method -> Cmd (Msg botMsg)
sendMethod token method =
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
    Http.post
        { url = getMethodUrl token methodName |> Url.toString
        , body = Http.jsonBody content
        , expect = Http.expectStringResponse (SentMethod method) parseSendMethodResponse
        }
