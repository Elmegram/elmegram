module Telegram.Test exposing (makeChat, makeMessage, makeUser, send, sendMessage)

import Telegram



-- HIGH-LEVEL


sendMessage : String -> Telegram.Update
sendMessage =
    makeMessage >> send



-- LOW-LEVEL


makeChat : Telegram.Chat
makeChat =
    { id = Telegram.makeTestId 1
    , type_ = Telegram.Private
    }


makeUser : Telegram.User
makeUser =
    { id = Telegram.makeTestId 1
    , is_bot = False
    , first_name = "Peter"
    , last_name = Nothing
    , username = Nothing
    , language_code = Nothing
    }


makeMessage : String -> Telegram.TextMessage
makeMessage text =
    { message_id = Telegram.makeTestId 1
    , date = 1
    , chat = makeChat
    , text = text
    , entities = parseEntities text
    }


parseEntities : String -> List Telegram.MessageEntity
parseEntities text =
    indexedWords text
        |> List.filterMap
            (\( index, word ) ->
                (if String.startsWith "/" word then
                    Just Telegram.BotCommand

                 else if String.startsWith "@" word then
                    Just Telegram.Mention

                 else if String.startsWith "#" word then
                    Just Telegram.Hashtag

                 else
                    Nothing
                )
                    |> (\constructor ->
                            let
                                length =
                                    String.length word

                                bounds =
                                    { offset = index, length = length }

                                maybeEntity =
                                    Maybe.map (\constr -> constr bounds) constructor
                            in
                            maybeEntity
                       )
            )


indexedWords : String -> List ( Int, String )
indexedWords text =
    let
        isWhitespace char =
            List.member char (String.toList " \t\n")
    in
    String.foldl
        (\char ( previous, ( currentWordStart, currentWord ), characterIndex ) ->
            if isWhitespace char then
                if String.isEmpty currentWord then
                    ( previous, ( characterIndex + 1, "" ), characterIndex + 1 )

                else
                    ( previous ++ [ ( currentWordStart, currentWord ) ], ( characterIndex + 1, "" ), characterIndex + 1 )

            else
                ( previous, ( currentWordStart, currentWord ++ String.fromChar char ), characterIndex + 1 )
        )
        ( [], ( 0, "" ), 0 )
        text
        |> (\( previous, last, _ ) ->
                let
                    lastWord =
                        Tuple.second last
                in
                if String.isEmpty lastWord then
                    previous

                else
                    previous ++ [ last ]
           )


send : Telegram.TextMessage -> Telegram.Update
send message =
    { update_id = Telegram.makeTestId 1
    , content = Telegram.MessageUpdate message
    }
