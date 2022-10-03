module Filter exposing
    ( Filter
    , Polarity(..)
    , decoder
    , defaultPolarity
    , dummy
    , encoder
    , filterType
    , filterTypes
    , isAllowed
    , ofType
    , polarityDecoder
    , polarityEncoder
    , updatePath
    , value
    )

import DecodeHelpers
import Json.Encode as JE
import List.Extra as LE
import TaskItem exposing (TaskItem)
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode


type Filter
    = FileFilter String
    | PathFilter String
    | TagFilter String


type Polarity
    = Allow
    | Deny


dummy : Filter
dummy =
    TagFilter ""


filterTypes : List String
filterTypes =
    [ "Files", "Paths", "Tags" ]


defaultPolarity : Polarity
defaultPolarity =
    Allow



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


polarityEncoder : TsEncode.Encoder Polarity
polarityEncoder =
    TsEncode.union
        (\vAllow vDeny v ->
            case v of
                Allow ->
                    vAllow

                Deny ->
                    vDeny
        )
        |> TsEncode.variantLiteral (JE.string "Allow")
        |> TsEncode.variantLiteral (JE.string "Deny")
        |> TsEncode.buildUnion


polarityDecoder : TsDecode.Decoder Polarity
polarityDecoder =
    TsDecode.oneOf
        [ TsDecode.literal Allow (JE.string "Allow")
        , TsDecode.literal Deny (JE.string "Deny")
        ]



-- INFO


ofType : String -> List Filter -> List Filter
ofType typeString filters =
    let
        isMatchingType : Filter -> Bool
        isMatchingType filter =
            case filter of
                FileFilter _ ->
                    typeString == "fileFilter"

                PathFilter _ ->
                    typeString == "pathFilter"

                TagFilter _ ->
                    typeString == "tagFilter"
    in
    List.filter isMatchingType filters


filterType : Filter -> String
filterType filter =
    case filter of
        FileFilter _ ->
            "Files"

        PathFilter _ ->
            "Paths"

        TagFilter _ ->
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
            TaskItem.hasThisTag tag taskItem



-- MODIFICATION


updatePath : String -> String -> Filter -> Filter
updatePath oldPath newPath filter =
    case filter of
        FileFilter filePath ->
            if filePath == oldPath then
                FileFilter newPath

            else
                filter

        PathFilter path ->
            if path == oldPath then
                PathFilter newPath

            else
                filter

        TagFilter _ ->
            filter



-- HELPERS


fileIsFromPath : String -> String -> Bool
fileIsFromPath file path =
    let
        pathComponents : List String
        pathComponents =
            path
                |> String.replace "\\" "/"
                |> String.split "/"
                |> List.filter (not << String.isEmpty)

        filePathComponents : List String
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
