module DefaultColumnNames exposing
    ( DefaultColumnNames
    , default
    , encoder
    , nameFor
    , v_0_10_0_decoder
    , v_0_11_0_decoder
    , v_0_7_0_decoder
    , v_0_8_0_decoder
    , v_0_9_0_decoder
    )

import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode



-- TYPES


type alias DefaultColumnNames =
    { today : Maybe String
    , tomorrow : Maybe String
    , future : Maybe String
    , undated : Maybe String
    , otherTags : Maybe String
    , untagged : Maybe String
    , completed : Maybe String
    }


default : DefaultColumnNames
default =
    { today = Nothing
    , tomorrow = Nothing
    , future = Nothing
    , undated = Nothing
    , otherTags = Nothing
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

        "otherTags" ->
            Maybe.withDefault "Other Tags" defaultColumnNames.otherTags

        "untagged" ->
            Maybe.withDefault "Untagged" defaultColumnNames.untagged

        "completed" ->
            Maybe.withDefault "Completed" defaultColumnNames.completed

        _ ->
            ""



-- SERIALIZE


v_0_11_0_decoder : TsDecode.Decoder DefaultColumnNames
v_0_11_0_decoder =
    TsDecode.succeed DefaultColumnNames
        |> TsDecode.andMap (TsDecode.field "today" <| TsDecode.map toMaybe TsDecode.string)
        |> TsDecode.andMap (TsDecode.field "tomorrow" <| TsDecode.map toMaybe TsDecode.string)
        |> TsDecode.andMap (TsDecode.field "future" <| TsDecode.map toMaybe TsDecode.string)
        |> TsDecode.andMap (TsDecode.field "undated" <| TsDecode.map toMaybe TsDecode.string)
        |> TsDecode.andMap (TsDecode.field "otherTags" <| TsDecode.map toMaybe TsDecode.string)
        |> TsDecode.andMap (TsDecode.field "untagged" <| TsDecode.map toMaybe TsDecode.string)
        |> TsDecode.andMap (TsDecode.field "completed" <| TsDecode.map toMaybe TsDecode.string)


v_0_10_0_decoder : TsDecode.Decoder DefaultColumnNames
v_0_10_0_decoder =
    v_0_9_0_decoder


v_0_9_0_decoder : TsDecode.Decoder DefaultColumnNames
v_0_9_0_decoder =
    v_0_8_0_decoder


v_0_8_0_decoder : TsDecode.Decoder DefaultColumnNames
v_0_8_0_decoder =
    v_0_7_0_decoder


v_0_7_0_decoder : TsDecode.Decoder DefaultColumnNames
v_0_7_0_decoder =
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
        , TsEncode.required "otherTags" (fromMaybe .otherTags) TsEncode.string
        , TsEncode.required "untagged" (fromMaybe .untagged) TsEncode.string
        , TsEncode.required "completed" (fromMaybe .completed) TsEncode.string
        ]



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
