module ColumnNames exposing
    ( ColumnNames
    , decoder
    , default
    , encoder
    , updateColumnName
    )

import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode



-- TYPES


type alias ColumnNames =
    { today : Maybe String
    , tomorrow : Maybe String
    , future : Maybe String
    , undated : Maybe String
    , others : Maybe String
    , untagged : Maybe String
    , completed : Maybe String
    }


default : ColumnNames
default =
    { today = Nothing
    , tomorrow = Nothing
    , future = Nothing
    , undated = Nothing
    , others = Nothing
    , untagged = Nothing
    , completed = Nothing
    }



-- SERIALIZE


decoder : TsDecode.Decoder ColumnNames
decoder =
    TsDecode.succeed ColumnNames
        |> TsDecode.andMap (TsDecode.field "today" <| TsDecode.map toMaybe TsDecode.string)
        |> TsDecode.andMap (TsDecode.field "tomorrow" <| TsDecode.map toMaybe TsDecode.string)
        |> TsDecode.andMap (TsDecode.field "future" <| TsDecode.map toMaybe TsDecode.string)
        |> TsDecode.andMap (TsDecode.field "undated" <| TsDecode.map toMaybe TsDecode.string)
        |> TsDecode.andMap (TsDecode.field "others" <| TsDecode.map toMaybe TsDecode.string)
        |> TsDecode.andMap (TsDecode.field "untagged" <| TsDecode.map toMaybe TsDecode.string)
        |> TsDecode.andMap (TsDecode.field "completed" <| TsDecode.map toMaybe TsDecode.string)


encoder : TsEncode.Encoder ColumnNames
encoder =
    TsEncode.object
        [ TsEncode.required "today" (fromMaybe .today) TsEncode.string
        , TsEncode.required "tomorrow" (fromMaybe .tomorrow) TsEncode.string
        , TsEncode.required "future" (fromMaybe .future) TsEncode.string
        , TsEncode.required "undated" (fromMaybe .undated) TsEncode.string
        , TsEncode.required "others" (fromMaybe .others) TsEncode.string
        , TsEncode.required "untagged" (fromMaybe .untagged) TsEncode.string
        , TsEncode.required "completed" (fromMaybe .completed) TsEncode.string
        ]



-- MODIFICATION


updateColumnName : String -> String -> ColumnNames -> ColumnNames
updateColumnName column newName columnNames =
    case column of
        "today" ->
            { columnNames | today = toMaybe newName }

        "tomorrow" ->
            { columnNames | tomorrow = toMaybe newName }

        "future" ->
            { columnNames | future = toMaybe newName }

        "undated" ->
            { columnNames | undated = toMaybe newName }

        "others" ->
            { columnNames | others = toMaybe newName }

        "untagged" ->
            { columnNames | untagged = toMaybe newName }

        "completed" ->
            { columnNames | completed = toMaybe newName }

        _ ->
            columnNames



-- PRIVATE


fromMaybe : (ColumnNames -> Maybe String) -> ColumnNames -> String
fromMaybe mapper =
    Maybe.withDefault "" << mapper


toMaybe : String -> Maybe String
toMaybe s =
    if String.length s == 0 then
        Nothing

    else
        Just s
