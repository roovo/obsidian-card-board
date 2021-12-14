module Filter exposing
    ( Filter(..)
    , decoder
    , encoder
    , filterType
    , filterTypes
    , isAllowed
    , ofType
    , value
    )

import DecodeHelpers
import TaskItem exposing (TaskItem)
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode


type Filter
    = FileFilter String
    | PathFilter String
    | TagFilter String


filterTypes : List String
filterTypes =
    [ "Files", "Paths", "Tags" ]



-- SERIALISATION


encoder : TsEncode.Encoder Filter
encoder =
    TsEncode.union
        (\vFileFilter vPathFilter vTagFilter v ->
            case v of
                FileFilter path ->
                    vFileFilter path

                PathFilter path ->
                    vPathFilter path

                TagFilter tag ->
                    vTagFilter tag
        )
        |> TsEncode.variantTagged "fileFilter" TsEncode.string
        |> TsEncode.variantTagged "pathFilter" TsEncode.string
        |> TsEncode.variantTagged "tagFilter" TsEncode.string
        |> TsEncode.buildUnion


decoder : TsDecode.Decoder Filter
decoder =
    TsDecode.oneOf
        [ DecodeHelpers.toElmVariant "fileFilter" FileFilter TsDecode.string
        , DecodeHelpers.toElmVariant "pathFilter" PathFilter TsDecode.string
        , DecodeHelpers.toElmVariant "tagFilter" TagFilter TsDecode.string
        ]



-- INFO


ofType : String -> List Filter -> List Filter
ofType typeString filters =
    let
        foo filter =
            case filter of
                FileFilter _ ->
                    typeString == "fileFilter"

                PathFilter _ ->
                    typeString == "pathFilter"

                TagFilter _ ->
                    typeString == "tagFilter"
    in
    List.filter foo filters


filterType : Filter -> String
filterType filter =
    case filter of
        FileFilter f ->
            "Files"

        PathFilter f ->
            "Paths"

        TagFilter f ->
            "Tags"


value : Filter -> String
value filter =
    case filter of
        FileFilter f ->
            f

        PathFilter f ->
            f

        TagFilter f ->
            f


isAllowed : TaskItem -> Filter -> Bool
isAllowed taskItem filter =
    case filter of
        FileFilter f ->
            TaskItem.isFromFile f taskItem

        PathFilter f ->
            True

        TagFilter f ->
            True
