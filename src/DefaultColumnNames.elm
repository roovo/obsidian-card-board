module DefaultColumnNames exposing
    ( DefaultColumnNames
    , decoder
    , default
    , encoder
    , nameFor
    , updateColumnName
    )

import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode



-- TYPES


type alias DefaultColumnNames =
    { today : Maybe String
    , tomorrow : Maybe String
    , future : Maybe String
    , undated : Maybe String
    , others : Maybe String
    , untagged : Maybe String
    , completed : Maybe String
    }


default : DefaultColumnNames
default =
    { today = Nothing
    , tomorrow = Nothing
    , future = Nothing
    , undated = Nothing
    , others = Nothing
    , untagged = Nothing
    , completed = Nothing
    }



-- INFO


nameFor : String -> DefaultColumnNames -> String
nameFor column defaultColumnNames =
    case column of
        "today" ->
            Maybe.withDefault "Today" defaultColumnNames.today

        "tomorrow" ->
            Maybe.withDefault "Tomorrow" defaultColumnNames.tomorrow

        "future" ->
            Maybe.withDefault "Future" defaultColumnNames.future

        "undated" ->
            Maybe.withDefault "Undated" defaultColumnNames.undated

        "others" ->
            Maybe.withDefault "Others" defaultColumnNames.others

        "untagged" ->
            Maybe.withDefault "Untagged" defaultColumnNames.untagged

        "completed" ->
            Maybe.withDefault "Completed" defaultColumnNames.completed

        _ ->
            ""



-- SERIALIZE


decoder : TsDecode.Decoder DefaultColumnNames
decoder =
    TsDecode.succeed DefaultColumnNames
        |> TsDecode.andMap (TsDecode.field "today" <| TsDecode.map toMaybe TsDecode.string)
        |> TsDecode.andMap (TsDecode.field "tomorrow" <| TsDecode.map toMaybe TsDecode.string)
        |> TsDecode.andMap (TsDecode.field "future" <| TsDecode.map toMaybe TsDecode.string)
        |> TsDecode.andMap (TsDecode.field "undated" <| TsDecode.map toMaybe TsDecode.string)
        |> TsDecode.andMap (TsDecode.field "others" <| TsDecode.map toMaybe TsDecode.string)
        |> TsDecode.andMap (TsDecode.field "untagged" <| TsDecode.map toMaybe TsDecode.string)
        |> TsDecode.andMap (TsDecode.field "completed" <| TsDecode.map toMaybe TsDecode.string)


encoder : TsEncode.Encoder DefaultColumnNames
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


updateColumnName : String -> String -> DefaultColumnNames -> DefaultColumnNames
updateColumnName column newName defaultColumnNames =
    case column of
        "today" ->
            { defaultColumnNames | today = toMaybe newName }

        "tomorrow" ->
            { defaultColumnNames | tomorrow = toMaybe newName }

        "future" ->
            { defaultColumnNames | future = toMaybe newName }

        "undated" ->
            { defaultColumnNames | undated = toMaybe newName }

        "others" ->
            { defaultColumnNames | others = toMaybe newName }

        "untagged" ->
            { defaultColumnNames | untagged = toMaybe newName }

        "completed" ->
            { defaultColumnNames | completed = toMaybe newName }

        _ ->
            defaultColumnNames



-- PRIVATE


fromMaybe : (DefaultColumnNames -> Maybe String) -> DefaultColumnNames -> String
fromMaybe mapper =
    Maybe.withDefault "" << mapper


toMaybe : String -> Maybe String
toMaybe s =
    if String.length s == 0 then
        Nothing

    else
        Just s
