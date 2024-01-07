module Filter exposing
    ( Filter(..)
    , Polarity(..)
    , Scope(..)
    , decoder
    , defaultPolarity
    , defaultScope
    , dummy
    , encoder
    , filterType
    , filterTypes
    , ofType
    , polarityDecoder
    , polarityEncoder
    , polarityFromString
    , scopeDecoder
    , scopeEncoder
    , updatePath
    , value
    )

import DecodeHelpers
import Json.Encode as JE
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode


type Filter
    = FileFilter String
    | PathFilter String
    | TagFilter String


type Polarity
    = Allow
    | Deny


type Scope
    = TopLevelOnly
    | SubTasksOnly
    | Both


dummy : Filter
dummy =
    TagFilter ""


filterTypes : List String
filterTypes =
    [ "Files", "Paths", "Tags" ]


defaultPolarity : Polarity
defaultPolarity =
    Allow


defaultScope : Scope
defaultScope =
    Both



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


polarityFromString : String -> Polarity
polarityFromString source =
    if source == "Deny" then
        Deny

    else
        Allow


scopeEncoder : TsEncode.Encoder Scope
scopeEncoder =
    TsEncode.union
        (\vTopLevelOnly vSubTasksOnly vBoth v ->
            case v of
                TopLevelOnly ->
                    vTopLevelOnly

                SubTasksOnly ->
                    vSubTasksOnly

                Both ->
                    vBoth
        )
        |> TsEncode.variantLiteral (JE.string "TopLevelOnly")
        |> TsEncode.variantLiteral (JE.string "SubTasksOnly")
        |> TsEncode.variantLiteral (JE.string "Both")
        |> TsEncode.buildUnion


scopeDecoder : TsDecode.Decoder Scope
scopeDecoder =
    TsDecode.oneOf
        [ TsDecode.literal TopLevelOnly (JE.string "TopLevelOnly")
        , TsDecode.literal SubTasksOnly (JE.string "SubTasksOnly")
        , TsDecode.literal Both (JE.string "Both")
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
