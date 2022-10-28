module DataviewTaskCompletion exposing
    ( DataviewTaskCompletion(..)
    , decoder
    )

import TsJson.Decode as TsDecode



-- TYPES


type DataviewTaskCompletion
    = NoCompletion
    | Emoji
    | Text String



-- SERIALIZE


decoder : TsDecode.Decoder DataviewTaskCompletion
decoder =
    TsDecode.succeed builder
        |> TsDecode.andMap (TsDecode.field "taskCompletionTracking" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "taskCompletionUseEmojiShorthand" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "taskCompletionText" TsDecode.string)



-- PRIVATE


builder : Bool -> Bool -> String -> DataviewTaskCompletion
builder tracking emoji text =
    case ( tracking, emoji, text ) of
        ( False, _, _ ) ->
            NoCompletion

        ( True, True, _ ) ->
            Emoji

        ( True, False, t ) ->
            Text t
