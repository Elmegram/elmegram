module Elmegram.Runner exposing
    ( ErrorPort
    , IncomingUpdatePort
    , botRunner
    )

import Elmegram.Bot as Bot exposing (Bot)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Telegram
import Url exposing (Url)



-- INTERFACE


type alias IncomingUpdatePort msg =
    (Encode.Value -> msg) -> Sub msg


type alias ErrorPort msg =
    String -> Cmd msg


botRunner :
    Bot model msg
    -> IncomingUpdatePort (Msg msg)
    -> ErrorPort (Msg msg)
    -> Platform.Program Encode.Value (Model model) (Msg msg)
botRunner bot incomingUpdatePort errorPort =
    Platform.worker
        { init = init errorPort
        , update = update bot errorPort
        , subscriptions = subscriptions incomingUpdatePort
        }



-- INIT


type alias Token =
    String


type alias Model botModel =
    Maybe
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
    | NewUpdate UpdateResult
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
                    ( Just
                        { botModel = bot.init self |> .model
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
                    processUpdate model.token bot.newUpdateMsg bot.update errorPort result model.botModel
                        |> Tuple.mapFirst (\botModel -> Just { model | botModel = botModel })

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


type alias UpdateResult =
    Result Decode.Error Telegram.Update


processUpdate :
    Token
    -> Bot.BotNewUpdateMsg botMsg
    -> Bot.BotUpdate model botMsg
    -> ErrorPort (Msg botMsg)
    -> UpdateResult
    -> model
    -> ( model, Cmd (Msg botMsg) )
processUpdate token botNewUpdateMsg botUpdate errorPort result model =
    case result of
        Err err ->
            ( model, Decode.errorToString err |> errorPort )

        Ok newUpdate ->
            botUpdate (botNewUpdateMsg newUpdate) model
                |> updateFromResponse token


updateFromResponse : Token -> Bot.Response model botMsg -> ( model, Cmd (Msg botMsg) )
updateFromResponse token response =
    ( response.model, cmdFromResponse token response )


cmdFromResponse : Token -> Bot.Response model botMsg -> Cmd (Msg botMsg)
cmdFromResponse token response =
    Cmd.batch
        ([ Cmd.map BotMsg response.command
         ]
            ++ (if List.isEmpty response.methods then
                    []

                else
                    List.map (sendMethod token) response.methods
               )
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



-- SUBSCRIPTIONS


subscriptions : IncomingUpdatePort (Msg botMsg) -> model -> Sub (Msg botMsg)
subscriptions incomingUpdatePort model =
    incomingUpdatePort (Decode.decodeValue Telegram.decodeUpdate >> NewUpdate)
