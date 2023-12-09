module TagBoardConfig exposing
    ( LocalColumnConfig
    , TagBoardConfig
    , collapseColumn
    , columnConfigsParser
    , completedCount
    , decoder_v_0_10_0
    , decoder_v_0_11_0
    , decoder_v_0_1_0
    , decoder_v_0_2_0
    , decoder_v_0_3_0
    , decoder_v_0_4_0
    , decoder_v_0_5_0
    , decoder_v_0_6_0
    , decoder_v_0_7_0
    , decoder_v_0_8_0
    , decoder_v_0_9_0
    , default
    , displayOthers
    , displayUntagged
    , encoder
    , setNamesToDefault
    , tagsToHide
    , toggleIncludeOthers
    , toggleIncludeUntagged
    , updateCompletedCount
    , updateTags
    )

import CollapsedColumns exposing (CollapsedColumns)
import Column exposing (Column)
import Column.Completed as CompletedColumn exposing (CompletedColumn)
import Column.OtherTags as OtherTagsColumn
import Columns exposing (Columns)
import DefaultColumnNames exposing (DefaultColumnNames)
import Filter exposing (Filter, Polarity, Scope)
import Parser as P exposing ((|.), (|=), Parser)
import ParserHelper
import String.Extra as SE
import TsJson.Decode as TsDecode
import TsJson.Decode.Pipeline as TsDecode
import TsJson.Encode as TsEncode



-- TYPES


type alias TagBoardConfig =
    { columns : Columns
    , filters : List Filter
    , filterPolarity : Polarity
    , filterScope : Scope
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
    { columns =
        Columns.fromList
            [ Column.completed <| CompletedColumn.init "Completed" 0 10
            ]
    , filters = []
    , filterPolarity = Filter.defaultPolarity
    , filterScope = Filter.defaultScope
    , showColumnTags = True
    , showFilteredTags = True
    , title = ""
    }



-- SERIALIZE


encoder : TsEncode.Encoder TagBoardConfig
encoder =
    TsEncode.object
        [ TsEncode.required "columns" .columns Columns.encoder
        , TsEncode.required "filters" .filters <| TsEncode.list Filter.encoder
        , TsEncode.required "filterPolarity" .filterPolarity Filter.polarityEncoder
        , TsEncode.required "filterScope" .filterScope Filter.scopeEncoder
        , TsEncode.required "showColumnTags" .showColumnTags TsEncode.bool
        , TsEncode.required "showFilteredTags" .showFilteredTags TsEncode.bool
        , TsEncode.required "title" .title TsEncode.string
        ]


columnConfigEncoder : TsEncode.Encoder LocalColumnConfig
columnConfigEncoder =
    TsEncode.object
        [ TsEncode.required "tag" .tag TsEncode.string
        , TsEncode.required "displayTitle" .displayTitle TsEncode.string
        ]


decoder_v_0_11_0 : TsDecode.Decoder TagBoardConfig
decoder_v_0_11_0 =
    (TsDecode.succeed TagBoardConfig
        |> TsDecode.required "columns" Columns.decoder
        |> TsDecode.required "filters" (TsDecode.list Filter.decoder)
        |> TsDecode.required "filterPolarity" Filter.polarityDecoder
        |> TsDecode.required "filterScope" Filter.scopeDecoder
        |> TsDecode.required "showColumnTags" TsDecode.bool
        |> TsDecode.required "showFilteredTags" TsDecode.bool
        |> TsDecode.required "title" TsDecode.string
    )
        |> TsDecode.map configureOtherTagsColumn


decoder_v_0_10_0 : TsDecode.Decoder TagBoardConfig
decoder_v_0_10_0 =
    decoder_v_0_9_0


decoder_v_0_9_0 : TsDecode.Decoder TagBoardConfig
decoder_v_0_9_0 =
    (TsDecode.succeed buildfromPreV11
        |> TsDecode.required "columns" (TsDecode.list columnConfigDecoder)
        |> TsDecode.required "collapsedColumns" (TsDecode.list TsDecode.int)
        |> TsDecode.required "completedCount" TsDecode.int
        |> TsDecode.required "filters" (TsDecode.list Filter.decoder)
        |> TsDecode.required "filterPolarity" Filter.polarityDecoder
        |> TsDecode.required "filterScope" Filter.scopeDecoder
        |> TsDecode.required "includeOthers" TsDecode.bool
        |> TsDecode.required "includeUntagged" TsDecode.bool
        |> TsDecode.required "showColumnTags" TsDecode.bool
        |> TsDecode.required "showFilteredTags" TsDecode.bool
        |> TsDecode.required "title" TsDecode.string
    )
        |> TsDecode.map configureOtherTagsColumn


decoder_v_0_8_0 : TsDecode.Decoder TagBoardConfig
decoder_v_0_8_0 =
    (TsDecode.succeed buildfromPreV11
        |> TsDecode.required "columns" (TsDecode.list columnConfigDecoder)
        |> TsDecode.hardcoded []
        |> TsDecode.required "completedCount" TsDecode.int
        |> TsDecode.required "filters" (TsDecode.list Filter.decoder)
        |> TsDecode.required "filterPolarity" Filter.polarityDecoder
        |> TsDecode.required "filterScope" Filter.scopeDecoder
        |> TsDecode.required "includeOthers" TsDecode.bool
        |> TsDecode.required "includeUntagged" TsDecode.bool
        |> TsDecode.required "showColumnTags" TsDecode.bool
        |> TsDecode.required "showFilteredTags" TsDecode.bool
        |> TsDecode.required "title" TsDecode.string
    )
        |> TsDecode.map configureOtherTagsColumn


decoder_v_0_7_0 : TsDecode.Decoder TagBoardConfig
decoder_v_0_7_0 =
    decoder_v_0_6_0


decoder_v_0_6_0 : TsDecode.Decoder TagBoardConfig
decoder_v_0_6_0 =
    decoder_v_0_5_0


decoder_v_0_5_0 : TsDecode.Decoder TagBoardConfig
decoder_v_0_5_0 =
    decoder_v_0_4_0


decoder_v_0_4_0 : TsDecode.Decoder TagBoardConfig
decoder_v_0_4_0 =
    (TsDecode.succeed buildfromPreV11
        |> TsDecode.required "columns" (TsDecode.list columnConfigDecoder)
        |> TsDecode.hardcoded []
        |> TsDecode.required "completedCount" TsDecode.int
        |> TsDecode.required "filters" (TsDecode.list Filter.decoder)
        |> TsDecode.required "filterPolarity" Filter.polarityDecoder
        |> TsDecode.hardcoded Filter.Both
        |> TsDecode.required "includeOthers" TsDecode.bool
        |> TsDecode.required "includeUntagged" TsDecode.bool
        |> TsDecode.required "showColumnTags" TsDecode.bool
        |> TsDecode.required "showFilteredTags" TsDecode.bool
        |> TsDecode.required "title" TsDecode.string
    )
        |> TsDecode.map configureOtherTagsColumn


decoder_v_0_3_0 : TsDecode.Decoder TagBoardConfig
decoder_v_0_3_0 =
    (TsDecode.succeed buildfromPreV11
        |> TsDecode.required "columns" (TsDecode.list columnConfigDecoder)
        |> TsDecode.hardcoded []
        |> TsDecode.required "completedCount" TsDecode.int
        |> TsDecode.required "filters" (TsDecode.list Filter.decoder)
        |> TsDecode.required "filterPolarity" Filter.polarityDecoder
        |> TsDecode.hardcoded Filter.Both
        |> TsDecode.required "includeOthers" TsDecode.bool
        |> TsDecode.required "includeUntagged" TsDecode.bool
        |> TsDecode.hardcoded True
        |> TsDecode.hardcoded True
        |> TsDecode.required "title" TsDecode.string
    )
        |> TsDecode.map configureOtherTagsColumn


decoder_v_0_2_0 : TsDecode.Decoder TagBoardConfig
decoder_v_0_2_0 =
    (TsDecode.succeed buildfromPreV11
        |> TsDecode.required "columns" (TsDecode.list columnConfigDecoder)
        |> TsDecode.hardcoded []
        |> TsDecode.required "completedCount" TsDecode.int
        |> TsDecode.required "filters" (TsDecode.list Filter.decoder)
        |> TsDecode.hardcoded Filter.Allow
        |> TsDecode.hardcoded Filter.Both
        |> TsDecode.required "includeOthers" TsDecode.bool
        |> TsDecode.required "includeUntagged" TsDecode.bool
        |> TsDecode.hardcoded True
        |> TsDecode.hardcoded True
        |> TsDecode.required "title" TsDecode.string
    )
        |> TsDecode.map configureOtherTagsColumn


decoder_v_0_1_0 : TsDecode.Decoder TagBoardConfig
decoder_v_0_1_0 =
    (TsDecode.succeed buildfromPreV11
        |> TsDecode.required "columns" (TsDecode.list columnConfigDecoder)
        |> TsDecode.hardcoded []
        |> TsDecode.required "completedCount" TsDecode.int
        |> TsDecode.hardcoded []
        |> TsDecode.hardcoded Filter.Allow
        |> TsDecode.hardcoded Filter.Both
        |> TsDecode.required "includeOthers" TsDecode.bool
        |> TsDecode.required "includeUntagged" TsDecode.bool
        |> TsDecode.hardcoded True
        |> TsDecode.hardcoded True
        |> TsDecode.required "title" TsDecode.string
    )
        |> TsDecode.map configureOtherTagsColumn



-- PARSE


columnConfigsParser : Parser (List LocalColumnConfig)
columnConfigsParser =
    P.loop [] columnConfigHelp



-- INFO


completedCount : TagBoardConfig -> Int
completedCount tagBoardConfig =
    Columns.completedCount tagBoardConfig.columns


displayOthers : TagBoardConfig -> Bool
displayOthers tagBoardConfig =
    Columns.includesOtherTags tagBoardConfig.columns


displayUntagged : TagBoardConfig -> Bool
displayUntagged tagBoardConfig =
    Columns.includesUntagged tagBoardConfig.columns


tagsToHide : TagBoardConfig -> List String
tagsToHide tagBoardConfig =
    let
        columnTagsToHide : List String
        columnTagsToHide =
            if tagBoardConfig.showColumnTags then
                []

            else
                -- tagBoardConfig
                --     |> .columns
                --     |> List.map .tag
                Columns.namedTagColumnTags tagBoardConfig.columns

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


collapseColumn : Int -> Bool -> TagBoardConfig -> TagBoardConfig
collapseColumn columnIndex isCollapsed tagBoardConfig =
    { tagBoardConfig | columns = Columns.collapseColumn columnIndex isCollapsed tagBoardConfig.columns }


toggleIncludeOthers : TagBoardConfig -> TagBoardConfig
toggleIncludeOthers tagBoardConfig =
    { tagBoardConfig | columns = Columns.toggleIncludeOthers tagBoardConfig.columns }


toggleIncludeUntagged : TagBoardConfig -> TagBoardConfig
toggleIncludeUntagged tagBoardConfig =
    { tagBoardConfig | columns = Columns.toggleIncludeUntagged tagBoardConfig.columns }


setNamesToDefault : DefaultColumnNames -> TagBoardConfig -> TagBoardConfig
setNamesToDefault defaultColumnNames tagBoardConfig =
    { tagBoardConfig | columns = Columns.setNamesToDefault defaultColumnNames tagBoardConfig.columns }


updateCompletedCount : Int -> TagBoardConfig -> TagBoardConfig
updateCompletedCount newCount tagBoardConfig =
    { tagBoardConfig | columns = Columns.updateCompletedCount newCount tagBoardConfig.columns }


updateTags : String -> TagBoardConfig -> TagBoardConfig
updateTags tags tagBoardConfig =
    let
        columnsConfig : Result (List P.DeadEnd) (List LocalColumnConfig)
        columnsConfig =
            P.run columnConfigsParser tags
    in
    case columnsConfig of
        Ok localColumnConfigs ->
            let
                newNamedTagColumns =
                    localColumnConfigs
                        |> List.map (\c -> Column.namedTag c.displayTitle c.tag)
            in
            { tagBoardConfig | columns = Columns.replaceNamedTagColumns newNamedTagColumns tagBoardConfig.columns }

        _ ->
            tagBoardConfig



-- PRIVATE


buildfromPreV11 : List LocalColumnConfig -> List Int -> Int -> List Filter -> Polarity -> Scope -> Bool -> Bool -> Bool -> Bool -> String -> TagBoardConfig
buildfromPreV11 localColumnConfigs collapsedColumns completedCount_ filters filterPolarity filterScope includeOthers includeUntagged showColumnTags showFilteredTags title =
    let
        columns =
            untaggedColumn
                ++ otherTagsColumn
                ++ namedTagColumns
                ++ completedColumn
                |> List.indexedMap handleCollapse

        completedColumn =
            if completedCount_ > 0 then
                [ Column.completed <| CompletedColumn.init "Completed" completedColumnIndex completedCount_ ]

            else
                []

        completedColumnIndex =
            List.length (untaggedColumn ++ otherTagsColumn ++ namedTagColumns)

        handleCollapse : Int -> Column -> Column
        handleCollapse index column =
            if List.member index collapsedColumns then
                Column.toggleCollapse column

            else
                column

        namedTagColumns =
            localColumnConfigs
                |> List.map (\c -> Column.namedTag c.displayTitle c.tag)

        otherTags =
            localColumnConfigs
                |> List.map .tag

        otherTagsColumn =
            if includeOthers then
                [ Column.otherTags "Others" otherTags ]

            else
                []

        untaggedColumn =
            if includeUntagged then
                [ Column.untagged "Untagged" ]

            else
                []
    in
    { columns = Columns.fromList columns
    , filters = filters
    , filterPolarity = filterPolarity
    , filterScope = filterScope
    , showColumnTags = showColumnTags
    , showFilteredTags = showFilteredTags
    , title = title
    }


columnConfigDecoder : TsDecode.Decoder LocalColumnConfig
columnConfigDecoder =
    TsDecode.succeed LocalColumnConfig
        |> TsDecode.andMap (TsDecode.field "tag" TsDecode.string)
        |> TsDecode.andMap (TsDecode.field "displayTitle" TsDecode.string)


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


configureOtherTagsColumn : TagBoardConfig -> TagBoardConfig
configureOtherTagsColumn tagBoardConfig =
    { tagBoardConfig
        | columns =
            Columns.updateOthers
                (OtherTagsColumn.setOtherTags <| Columns.namedTagColumnTags tagBoardConfig.columns)
                tagBoardConfig.columns
    }



-- tagBoardConfig
