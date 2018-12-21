module Elmegram exposing
    ( FormattedText
    , containsCommand
    , format
    , getDisplayName
    , inlineQueryResultArticle
    , inlineQueryResultFromArticle
    , makeAnswer
    , makeAnswerCallbackQuery
    , makeAnswerFormatted
    , makeAnswerInlineQuery
    , makeInputMessage
    , makeInputMessageFormatted
    , makeMinimalInlineQueryResultArticle
    , makeReply
    , makeReplyFormatted
    , matchesCommand
    )

import Json.Decode as Decode
import Json.Encode as Encode
import Telegram



-- MESSAGES


containsCommand : Telegram.TextMessage -> Bool
containsCommand message =
    List.any
        (\entity ->
            case entity of
                Telegram.BotCommand _ ->
                    True

                _ ->
                    False
        )
        message.entities


matchesCommand : String -> Telegram.TextMessage -> Bool
matchesCommand command message =
    List.any
        (\entity ->
            case entity of
                Telegram.BotCommand bounds ->
                    let
                        end =
                            bounds.offset + bounds.length
                    in
                    String.slice bounds.offset end message.text
                        |> -- Drop the '/'.
                           String.dropLeft 1
                        |> String.split "@"
                        |> List.head
                        |> Maybe.map (\actual -> actual == command)
                        |> Maybe.withDefault False

                _ ->
                    False
        )
        message.entities



-- SEND MESSAGES


makeAnswer : Telegram.Chat -> String -> Telegram.SendMessage
makeAnswer to text =
    { chat_id = to.id
    , text = text
    , parse_mode = Nothing
    , reply_to_message_id = Nothing
    , reply_markup = Nothing
    }


makeAnswerFormatted : Telegram.Chat -> FormattedText -> Telegram.SendMessage
makeAnswerFormatted to (Format mode text) =
    let
        sendMessage =
            makeAnswer to text
    in
    { sendMessage
        | parse_mode = Just mode
    }


type FormattedText
    = Format Telegram.ParseMode String


format : Telegram.ParseMode -> String -> FormattedText
format mode text =
    Format mode text


makeReply : Telegram.TextMessage -> String -> Telegram.SendMessage
makeReply to text =
    { chat_id = to.chat.id
    , text = text
    , parse_mode = Nothing
    , reply_to_message_id = Just to.message_id
    , reply_markup = Nothing
    }


makeReplyFormatted : Telegram.TextMessage -> FormattedText -> Telegram.SendMessage
makeReplyFormatted to (Format mode text) =
    let
        sendMessage =
            makeReply to text
    in
    { sendMessage
        | parse_mode = Just mode
    }



-- ANSWER INLINE QUERIES


makeAnswerInlineQuery : Telegram.InlineQuery -> List Telegram.InlineQueryResult -> Telegram.AnswerInlineQuery
makeAnswerInlineQuery to results =
    { inline_query_id = to.id
    , results = results
    , cache_time = Nothing
    , is_personal = Nothing
    , next_offset = Nothing
    , switch_pm = Nothing
    }


makeMinimalInlineQueryResultArticle : { a | id : String, title : String, message : Telegram.InputMessageContent } -> Telegram.InlineQueryResultArticle
makeMinimalInlineQueryResultArticle { id, title, message } =
    { id = id
    , title = title
    , input_message_content = message
    , description = Nothing
    , url = Nothing
    , thumb_url = Nothing
    , reply_markup = Nothing
    }


inlineQueryResultArticle : { id : String, title : String, description : String, message : Telegram.InputMessageContent } -> Telegram.InlineQueryResult
inlineQueryResultArticle config =
    let
        article =
            makeMinimalInlineQueryResultArticle config
    in
    { article
        | description = Just config.description
    }
        |> inlineQueryResultFromArticle


inlineQueryResultFromArticle =
    Telegram.Article


makeInputMessage : String -> Telegram.InputMessageContent
makeInputMessage text =
    Telegram.Text
        { message_text = text
        , parse_mode = Nothing
        }


makeInputMessageFormatted : FormattedText -> Telegram.InputMessageContent
makeInputMessageFormatted (Format mode text) =
    Telegram.Text
        { message_text = text
        , parse_mode = Just mode
        }



-- ANSWER CALLBACK QUERIES


makeAnswerCallbackQuery : Telegram.CallbackQuery -> Telegram.AnswerCallbackQuery
makeAnswerCallbackQuery to =
    { callback_query_id = to.id
    , text = Nothing
    , show_alert = False
    , url = Nothing
    , cache_time = 0
    }



-- USERS


getDisplayName : Telegram.User -> String
getDisplayName user =
    case user.username of
        Just username ->
            username

        Nothing ->
            case user.last_name of
                Just lastName ->
                    user.first_name ++ " " ++ lastName

                Nothing ->
                    user.first_name
