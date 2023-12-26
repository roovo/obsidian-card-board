module Form.GlobalSettings exposing
    ( Form
    , safeDecoder
    )

-- TYPES


type alias GlobalSettings =
    { taskCompletionFormat : String
    , today : String
    , tomorrow : String
    , future : String
    , undated : String
    , otherTags : String
    , untagged : String
    , completed : String
    , ignoreFileNameDates : Bool
    }


safeDecoder : TsDecode.Decoder GlobalSettings
safeDecoder =
    TsDecode.succeed GlobalSettings
        |> TsDecode.andMap (TsDecode.field "taskCompletionFormat" taskCompletionFormatDecoder)
        |> TsDecode.andMap (TsDecode.field "defaultColumnNames" DefaultColumnNames.v_0_11_0_decoder)
        |> TsDecode.andMap (TsDecode.field "ignoreFileNameDates" TsDecode.bool)
