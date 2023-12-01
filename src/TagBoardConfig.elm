module TagBoardConfig exposing
    ( LocalColumnConfig
    , TagBoardConfig
    , columnConfigsParser
    , decoder_v_0_10_0
    , decoder_v_0_1_0
    , decoder_v_0_2_0
    , decoder_v_0_3_0
    , decoder_v_0_4_0
    , decoder_v_0_5_0
    , decoder_v_0_9_0
    , default
    , encoder
    , tagsToHide
    , updateColumnNames
    )

import CollapsedColumns exposing (CollapsedColumns)
import Column exposing (Column)
import ColumnNames exposing (ColumnNames)
import Columns exposing (Columns)
import Filter exposing (Filter, Polarity, Scope)
import Parser as P exposing ((|.), (|=), Parser)
import ParserHelper
import String.Extra as SE
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode



-- TYPES


type alias TagBoardConfig =
    { columns : List LocalColumnConfig
    , columnConfigs : Columns
    , collapsedColumns : CollapsedColumns
    , completedCount : Int
    , filters : List Filter
    , filterPolarity : Polarity
    , filterScope : Scope
    , includeOthers : Bool
    , includeUntagged : Bool
    , showColumnTags : Bool
    , showFilteredTags : Bool
    , title : String
    }


type alias LocalColumnConfig =
    { tag : String
    , displayTitle : String
    }


default : TagBoardConfig
default =
    { columns = []
    , columnConfigs = Columns.legacyFromList ColumnNames.default [] 10
    , collapsedColumns = CollapsedColumns.init
    , completedCount = 10
    , filters = []
    , filterPolarity = Filter.defaultPolarity
    , filterScope = Filter.defaultScope
    , includeOthers = False
    , includeUntagged = False
    , showColumnTags = True
    , showFilteredTags = True
    , title = ""
    }



-- SERIALIZE


encoder : TsEncode.Encoder TagBoardConfig
encoder =
    TsEncode.object
        [ TsEncode.required "columns" .columns <| TsEncode.list columnConfigEncoder
        , TsEncode.required "showColumnTags" .showColumnTags TsEncode.bool
        , TsEncode.required "completedCount" .completedCount TsEncode.int
        , TsEncode.required "filters" .filters <| TsEncode.list Filter.encoder
        , TsEncode.required "filterPolarity" .filterPolarity Filter.polarityEncoder
        , TsEncode.required "filterScope" .filterScope Filter.scopeEncoder
        , TsEncode.required "showFilteredTags" .showFilteredTags TsEncode.bool
        , TsEncode.required "includeOthers" .includeOthers TsEncode.bool
        , TsEncode.required "includeUntagged" .includeUntagged TsEncode.bool
        , TsEncode.required "title" .title TsEncode.string
        , TsEncode.required "collapsedColumns" .collapsedColumns CollapsedColumns.encoder
        ]


columnConfigEncoder : TsEncode.Encoder LocalColumnConfig
columnConfigEncoder =
    TsEncode.object
        [ TsEncode.required "tag" .tag TsEncode.string
        , TsEncode.required "displayTitle" .displayTitle TsEncode.string
        ]


decoder_v_0_10_0 : TsDecode.Decoder TagBoardConfig
decoder_v_0_10_0 =
    decoder_v_0_9_0


decoder_v_0_9_0 : TsDecode.Decoder TagBoardConfig
decoder_v_0_9_0 =
    TsDecode.succeed TagBoardConfig
        |> TsDecode.andMap (TsDecode.field "columns" (TsDecode.list columnConfigDecoder))
        |> TsDecode.andMap (TsDecode.succeed Columns.empty)
        |> TsDecode.andMap (TsDecode.field "collapsedColumns" CollapsedColumns.decoder)
        |> TsDecode.andMap (TsDecode.field "completedCount" TsDecode.int)
        |> TsDecode.andMap (TsDecode.field "filters" <| TsDecode.list Filter.decoder)
        |> TsDecode.andMap (TsDecode.field "filterPolarity" <| Filter.polarityDecoder)
        |> TsDecode.andMap (TsDecode.field "filterScope" <| Filter.scopeDecoder)
        |> TsDecode.andMap (TsDecode.field "includeOthers" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "includeUntagged" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "showColumnTags" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "showFilteredTags" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "title" TsDecode.string)


decoder_v_0_5_0 : TsDecode.Decoder TagBoardConfig
decoder_v_0_5_0 =
    TsDecode.succeed TagBoardConfig
        |> TsDecode.andMap (TsDecode.field "columns" (TsDecode.list columnConfigDecoder))
        |> TsDecode.andMap (TsDecode.succeed Columns.empty)
        |> TsDecode.andMap (TsDecode.succeed CollapsedColumns.init)
        |> TsDecode.andMap (TsDecode.field "completedCount" TsDecode.int)
        |> TsDecode.andMap (TsDecode.field "filters" <| TsDecode.list Filter.decoder)
        |> TsDecode.andMap (TsDecode.field "filterPolarity" <| Filter.polarityDecoder)
        |> TsDecode.andMap (TsDecode.field "filterScope" <| Filter.scopeDecoder)
        |> TsDecode.andMap (TsDecode.field "includeOthers" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "includeUntagged" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "showColumnTags" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "showFilteredTags" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "title" TsDecode.string)


decoder_v_0_4_0 : TsDecode.Decoder TagBoardConfig
decoder_v_0_4_0 =
    TsDecode.succeed TagBoardConfig
        |> TsDecode.andMap (TsDecode.field "columns" (TsDecode.list columnConfigDecoder))
        |> TsDecode.andMap (TsDecode.succeed Columns.empty)
        |> TsDecode.andMap (TsDecode.succeed CollapsedColumns.init)
        |> TsDecode.andMap (TsDecode.field "completedCount" TsDecode.int)
        |> TsDecode.andMap (TsDecode.field "filters" <| TsDecode.list Filter.decoder)
        |> TsDecode.andMap (TsDecode.field "filterPolarity" <| Filter.polarityDecoder)
        |> TsDecode.andMap (TsDecode.succeed Filter.Both)
        |> TsDecode.andMap (TsDecode.field "includeOthers" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "includeUntagged" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "showColumnTags" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "showFilteredTags" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "title" TsDecode.string)


decoder_v_0_3_0 : TsDecode.Decoder TagBoardConfig
decoder_v_0_3_0 =
    TsDecode.succeed TagBoardConfig
        |> TsDecode.andMap (TsDecode.field "columns" (TsDecode.list columnConfigDecoder))
        |> TsDecode.andMap (TsDecode.succeed Columns.empty)
        |> TsDecode.andMap (TsDecode.succeed CollapsedColumns.init)
        |> TsDecode.andMap (TsDecode.field "completedCount" TsDecode.int)
        |> TsDecode.andMap (TsDecode.field "filters" <| TsDecode.list Filter.decoder)
        |> TsDecode.andMap (TsDecode.field "filterPolarity" <| Filter.polarityDecoder)
        |> TsDecode.andMap (TsDecode.succeed Filter.Both)
        |> TsDecode.andMap (TsDecode.field "includeOthers" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "includeUntagged" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.succeed True)
        |> TsDecode.andMap (TsDecode.succeed True)
        |> TsDecode.andMap (TsDecode.field "title" TsDecode.string)


decoder_v_0_2_0 : TsDecode.Decoder TagBoardConfig
decoder_v_0_2_0 =
    TsDecode.succeed TagBoardConfig
        |> TsDecode.andMap (TsDecode.field "columns" (TsDecode.list columnConfigDecoder))
        |> TsDecode.andMap (TsDecode.succeed Columns.empty)
        |> TsDecode.andMap (TsDecode.succeed CollapsedColumns.init)
        |> TsDecode.andMap (TsDecode.field "completedCount" TsDecode.int)
        |> TsDecode.andMap (TsDecode.field "filters" <| TsDecode.list Filter.decoder)
        |> TsDecode.andMap (TsDecode.succeed Filter.Allow)
        |> TsDecode.andMap (TsDecode.succeed Filter.Both)
        |> TsDecode.andMap (TsDecode.field "includeOthers" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "includeUntagged" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.succeed True)
        |> TsDecode.andMap (TsDecode.succeed True)
        |> TsDecode.andMap (TsDecode.field "title" TsDecode.string)


decoder_v_0_1_0 : TsDecode.Decoder TagBoardConfig
decoder_v_0_1_0 =
    TsDecode.succeed TagBoardConfig
        |> TsDecode.andMap (TsDecode.field "columns" (TsDecode.list columnConfigDecoder))
        |> TsDecode.andMap (TsDecode.succeed Columns.empty)
        |> TsDecode.andMap (TsDecode.succeed CollapsedColumns.init)
        |> TsDecode.andMap (TsDecode.field "completedCount" TsDecode.int)
        |> TsDecode.andMap (TsDecode.succeed [])
        |> TsDecode.andMap (TsDecode.succeed Filter.Allow)
        |> TsDecode.andMap (TsDecode.succeed Filter.Both)
        |> TsDecode.andMap (TsDecode.field "includeOthers" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "includeUntagged" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.succeed True)
        |> TsDecode.andMap (TsDecode.succeed True)
        |> TsDecode.andMap (TsDecode.field "title" TsDecode.string)


columnConfigDecoder : TsDecode.Decoder LocalColumnConfig
columnConfigDecoder =
    TsDecode.succeed LocalColumnConfig
        |> TsDecode.andMap (TsDecode.field "tag" TsDecode.string)
        |> TsDecode.andMap (TsDecode.field "displayTitle" TsDecode.string)



-- PARSE


columnConfigsParser : Parser (List LocalColumnConfig)
columnConfigsParser =
    P.loop [] columnConfigHelp



-- INFO


tagsToHide : TagBoardConfig -> List String
tagsToHide tagBoardConfig =
    let
        columnTagsToHide : List String
        columnTagsToHide =
            if tagBoardConfig.showColumnTags then
                []

            else
                tagBoardConfig
                    |> .columns
                    |> List.map .tag

        filterTagsToHide : List String
        filterTagsToHide =
            if tagBoardConfig.showFilteredTags then
                []

            else
                tagBoardConfig
                    |> .filters
                    |> List.filter (\f -> Filter.filterType f == "Tags")
                    |> List.map Filter.value
    in
    columnTagsToHide ++ filterTagsToHide



-- MODIFICATION


updateColumnNames : ColumnNames -> TagBoardConfig -> TagBoardConfig
updateColumnNames columnNames tagBoardConfig =
    let
        columnTags : List String
        columnTags =
            tagBoardConfig
                |> .columns
                |> List.map .tag

        tagColumns : List Column
        tagColumns =
            tagBoardConfig.columns
                |> List.map (\c -> Column.namedTag c.displayTitle c.tag)

        others : List Column
        others =
            if tagBoardConfig.includeOthers then
                [ Column.otherTags (ColumnNames.nameFor "others" columnNames) columnTags ]

            else
                []

        untagged : List Column
        untagged =
            if tagBoardConfig.includeUntagged then
                [ Column.untagged (ColumnNames.nameFor "untagged" columnNames) ]

            else
                []
    in
    { tagBoardConfig | columnConfigs = Columns.legacyFromList columnNames (untagged ++ others ++ tagColumns) tagBoardConfig.completedCount }



-- PRIVATE


columnConfigHelp : List LocalColumnConfig -> Parser (P.Step (List LocalColumnConfig) (List LocalColumnConfig))
columnConfigHelp revStmts =
    P.oneOf
        [ P.succeed (\stmt -> P.Loop (stmt :: revStmts))
            |= columnConfigParser
            |. P.spaces
        , P.succeed ()
            |> P.map (\_ -> P.Done (List.reverse revStmts))
        ]


columnConfigParser : Parser LocalColumnConfig
columnConfigParser =
    let
        buildColumnConfig : ( String, Maybe String ) -> Parser LocalColumnConfig
        buildColumnConfig ( tag, title ) =
            let
                cleanedTag : String
                cleanedTag =
                    if String.startsWith "#" tag then
                        String.dropLeft 1 tag

                    else
                        tag

                displayTitle : String
                displayTitle =
                    title
                        |> Maybe.withDefault defaultTitle
                        |> String.words
                        |> String.join " "

                defaultTitle : String
                defaultTitle =
                    cleanedTag
                        |> String.replace "/" " "
                        |> SE.toSentenceCase
            in
            P.succeed { tag = cleanedTag, displayTitle = displayTitle }
    in
    P.succeed Tuple.pair
        |. ParserHelper.spaces
        |= ParserHelper.wordParser
        |. ParserHelper.spaces
        |= P.oneOf
            [ P.map Just ParserHelper.nonEmptyStringParser
            , P.succeed Nothing
            ]
        |> P.andThen buildColumnConfig
