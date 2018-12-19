module Elmegram.Bot exposing (Bot, BotInit, BotNewUpdateMsg, BotUpdate, Method(..), Response, encodeMethod)

import Json.Encode as Encode
import Telegram


type alias Bot model msg =
    { init : BotInit model msg
    , newUpdateMsg : BotNewUpdateMsg msg
    , update : BotUpdate model msg
    }


type alias BotInit model msg =
    Telegram.User -> Response model msg


type alias BotNewUpdateMsg msg =
    Telegram.Update -> msg


type alias BotUpdate model msg =
    msg -> model -> Response model msg



-- RESPONSES


type alias Response model msg =
    { methods : List Method
    , model : model
    , command : Cmd msg
    }


type Method
    = SendMessageMethod Telegram.SendMessage
    | AnswerInlineQueryMethod Telegram.AnswerInlineQuery
    | AnswerCallbackQueryMethod Telegram.AnswerCallbackQuery


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
