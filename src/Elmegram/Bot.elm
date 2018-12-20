module Elmegram.Bot exposing (Bot, Init, Method(..), NewUpdateMsg, Response, Update, encodeMethod, sendMethod)

import Json.Encode as Encode
import Telegram
import Telegram.Methods as Telegram


type alias Bot model msg =
    { init : Init model msg
    , newUpdateMsg : NewUpdateMsg msg
    , update : Update model msg
    }


type alias Init model msg =
    Telegram.User -> Response model msg


type alias NewUpdateMsg msg =
    Telegram.Update -> msg


type alias Update model msg =
    msg -> model -> Response model msg



-- RESPONSES


type alias Response model msg =
    { methods : List Method
    , model : model
    , cmd : Cmd msg
    }


type Method
    = SendMessageMethod Telegram.SendMessage
    | AnswerInlineQueryMethod Telegram.AnswerInlineQuery
    | AnswerCallbackQueryMethod Telegram.AnswerCallbackQuery


sendMethod : Telegram.Token -> Method -> (Result String () -> msg) -> Cmd msg
sendMethod token method tagger =
    case method of
        SendMessageMethod toSend ->
            Telegram.sendMessage token toSend tagger

        AnswerInlineQueryMethod toSend ->
            Telegram.answerInlineQuery token toSend tagger

        AnswerCallbackQueryMethod toSend ->
            Telegram.answerCallbackQuery token toSend tagger


encodeMethod : Method -> { methodName : String, content : Encode.Value }
encodeMethod method =
    case method of
        SendMessageMethod sendMessage ->
            { methodName = "sendMessage"
            , content =
                Telegram.encodeSendMessage sendMessage
            }

        AnswerInlineQueryMethod inlineQuery ->
            { methodName = "answerInlineQuery"
            , content =
                Telegram.encodeAnswerInlineQuery inlineQuery
            }

        AnswerCallbackQueryMethod callbackQuery ->
            { methodName = "answerCallbackQuery"
            , content =
                Telegram.encodeAnswerCallbackQuery callbackQuery
            }
