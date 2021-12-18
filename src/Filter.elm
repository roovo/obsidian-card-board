module Filter exposing
    ( Filter
    , decoder
    , dummy
    , encoder
    , filterType
    , filterTypes
    , isAllowed
    , ofType
    , value
    )

import DecodeHelpers
import List.Extra as LE
import TaskItem exposing (TaskItem)
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode


type Filter
    = FileFilter String
    | PathFilter String
    | TagFilter String


dummy : Filter
dummy =
    TagFilter ""


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
        FileFilter filePath ->
            TaskItem.isFromFile filePath taskItem

        PathFilter path ->
            fileIsFromPath (TaskItem.filePath taskItem) path

        TagFilter tag ->
            TaskItem.hasTag tag taskItem


fileIsFromPath : String -> String -> Bool
fileIsFromPath file path =
    let
        pathComponents =
            path
                |> String.replace "\\" "/"
                |> String.split "/"
                |> List.filter (not << String.isEmpty)

        filePathComponents =
            file
                |> String.replace "\\" "/"
                |> String.split "/"
                |> List.reverse
                |> List.drop 1
                |> List.reverse
                |> List.filter (not << String.isEmpty)

        isComponentMatching : Int -> String -> Bool
        isComponentMatching index pathComponent =
            case LE.getAt index filePathComponents of
                Nothing ->
                    False

                Just filePathComponent ->
                    filePathComponent == pathComponent
    in
    pathComponents
        |> List.indexedMap isComponentMatching
        |> List.all identity
