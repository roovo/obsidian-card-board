module BoardConfig exposing
    ( BoardConfig(..)
    , Config
    , addColumn
    , collapseColumn
    , columns
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
    , encoder
    , filterPolarity
    , filterScope
    , filters
    , fromNewBoardConfig
    , mapFilters
    , name
    , setNamesToDefault
    , showColumnTags
    , showFilteredTags
    , toggleShowColumnTags
    , toggleShowFilteredTags
    , toggleTagFilterScope
    , updateColumnName
    , updateCompletedColumnLimit
    , updateDatedColumnRangeType
    , updateDatedColumnRangeValueFrom
    , updateDatedColumnRangeValueTo
    , updateFilterPolarity
    , updateFilterScope
    , updateFilters
    , updateName
    )

import Column exposing (Column)
import Column.Completed as CompletedColumn
import Column.Dated as DatedColumn
import Column.OtherTags as OtherTagsColumn
import Columns exposing (Columns)
import DecodeHelpers
import DefaultColumnNames exposing (DefaultColumnNames)
import Filter exposing (Filter, Polarity, Scope)
import NewBoardConfig exposing (NewBoardConfig)
import NewColumnConfig exposing (NewColumnConfig)
import TsJson.Decode as TsDecode
import TsJson.Decode.Pipeline as TsDecode
import TsJson.Encode as TsEncode



-- TYPES


type BoardConfig
    = BoardConfig Config


type alias Config =
    { columns : Columns
    , filters : List Filter
    , filterPolarity : Polarity
    , filterScope : Scope
    , name : String
    , showColumnTags : Bool
    , showFilteredTags : Bool
    }


type alias LocalColumnConfig =
    { tag : String
    , displayName : String
    }



-- CREATE


fromNewBoardConfig : DefaultColumnNames -> NewBoardConfig -> BoardConfig
fromNewBoardConfig defaultColumnNames newBoardConfig =
    case newBoardConfig.boardType of
        "dateBoard" ->
            BoardConfig
                { columns =
                    Columns.fromList
                        [ Column.undated (DefaultColumnNames.nameFor "undated" defaultColumnNames)
                        , Column.dated <| DatedColumn.init (DefaultColumnNames.nameFor "today" defaultColumnNames) (DatedColumn.Before 1)
                        , Column.dated <| DatedColumn.init (DefaultColumnNames.nameFor "tomorrow" defaultColumnNames) (DatedColumn.Between { from = 1, to = 1 })
                        , Column.dated <| DatedColumn.init (DefaultColumnNames.nameFor "future" defaultColumnNames) (DatedColumn.After 1)
                        , Column.completed <| CompletedColumn.init (DefaultColumnNames.nameFor "completed" defaultColumnNames) 4 10
                        ]
                , filters = []
                , filterPolarity = Filter.defaultPolarity
                , filterScope = Filter.defaultScope
                , name = newBoardConfig.name
                , showColumnTags = True
                , showFilteredTags = True
                }

        "tagBoard" ->
            BoardConfig
                { columns =
                    Columns.fromList
                        [ Column.untagged (DefaultColumnNames.nameFor "untagged" defaultColumnNames)
                        , Column.otherTags (DefaultColumnNames.nameFor "otherTags" defaultColumnNames) []
                        , Column.completed <| CompletedColumn.init (DefaultColumnNames.nameFor "completed" defaultColumnNames) 2 10
                        ]
                , filters = []
                , filterPolarity = Filter.defaultPolarity
                , filterScope = Filter.defaultScope
                , name = newBoardConfig.name
                , showColumnTags = True
                , showFilteredTags = True
                }

        _ ->
            BoardConfig
                { columns = Columns.empty
                , filters = []
                , filterPolarity = Filter.defaultPolarity
                , filterScope = Filter.defaultScope
                , name = newBoardConfig.name
                , showColumnTags = True
                , showFilteredTags = True
                }



-- UTILITIES


columns : BoardConfig -> Columns
columns =
    .columns << config


filters : BoardConfig -> List Filter
filters =
    .filters << config


filterPolarity : BoardConfig -> Polarity
filterPolarity =
    .filterPolarity << config


filterScope : BoardConfig -> Scope
filterScope =
    .filterScope << config


showColumnTags : BoardConfig -> Bool
showColumnTags =
    .showColumnTags << config


showFilteredTags : BoardConfig -> Bool
showFilteredTags =
    .showFilteredTags << config


name : BoardConfig -> String
name =
    .name << config



-- SERIALIZE


encoder : TsEncode.Encoder BoardConfig
encoder =
    TsEncode.map config <|
        TsEncode.object
            [ TsEncode.required "columns" .columns Columns.encoder
            , TsEncode.required "filters" .filters <| TsEncode.list Filter.encoder
            , TsEncode.required "filterPolarity" .filterPolarity Filter.polarityEncoder
            , TsEncode.required "filterScope" .filterScope Filter.scopeEncoder
            , TsEncode.required "name" .name TsEncode.string
            , TsEncode.required "showColumnTags" .showColumnTags TsEncode.bool
            , TsEncode.required "showFilteredTags" .showFilteredTags TsEncode.bool
            ]


decoder_v_0_11_0 : TsDecode.Decoder BoardConfig
decoder_v_0_11_0 =
    (TsDecode.succeed Config
        |> TsDecode.required "columns" Columns.decoder
        |> TsDecode.required "filters" (TsDecode.list Filter.decoder)
        |> TsDecode.required "filterPolarity" Filter.polarityDecoder
        |> TsDecode.required "filterScope" Filter.scopeDecoder
        |> TsDecode.required "name" TsDecode.string
        |> TsDecode.required "showColumnTags" TsDecode.bool
        |> TsDecode.required "showFilteredTags" TsDecode.bool
    )
        |> TsDecode.map configureOtherTagsColumn
        |> TsDecode.map BoardConfig



-- TRANSFORM


addColumn : DefaultColumnNames -> NewColumnConfig -> BoardConfig -> BoardConfig
addColumn defaultColumnNames newColumnConfig (BoardConfig c) =
    BoardConfig { c | columns = Columns.addColumn defaultColumnNames newColumnConfig c.columns }


mapFilters : (Filter -> Filter) -> BoardConfig -> BoardConfig
mapFilters fn (BoardConfig c) =
    BoardConfig { c | filters = List.map fn c.filters }


collapseColumn : Int -> Bool -> BoardConfig -> BoardConfig
collapseColumn columnIndex isCollapsed (BoardConfig c) =
    BoardConfig { c | columns = Columns.collapseColumn columnIndex isCollapsed c.columns }


setNamesToDefault : DefaultColumnNames -> BoardConfig -> BoardConfig
setNamesToDefault defaultColumnNames (BoardConfig c) =
    BoardConfig { c | columns = Columns.setNamesToDefault defaultColumnNames c.columns }


toggleShowColumnTags : BoardConfig -> BoardConfig
toggleShowColumnTags (BoardConfig c) =
    BoardConfig { c | showColumnTags = not c.showColumnTags }


toggleShowFilteredTags : BoardConfig -> BoardConfig
toggleShowFilteredTags (BoardConfig c) =
    BoardConfig { c | showFilteredTags = not c.showFilteredTags }


toggleTagFilterScope : BoardConfig -> BoardConfig
toggleTagFilterScope (BoardConfig c) =
    let
        cycleScope : Scope -> Scope
        cycleScope scope =
            case scope of
                Filter.TopLevelOnly ->
                    Filter.SubTasksOnly

                Filter.SubTasksOnly ->
                    Filter.Both

                Filter.Both ->
                    Filter.TopLevelOnly
    in
    BoardConfig { c | filterScope = cycleScope c.filterScope }


updateColumnName : Int -> String -> BoardConfig -> BoardConfig
updateColumnName index newName (BoardConfig c) =
    BoardConfig { c | columns = Columns.updateColumnName index newName c.columns }


updateCompletedColumnLimit : Int -> Maybe Int -> BoardConfig -> BoardConfig
updateCompletedColumnLimit index value ((BoardConfig c) as boardConfig) =
    case value of
        Just newLimit ->
            BoardConfig { c | columns = Columns.updateCompletedColumnLimit index newLimit c.columns }

        _ ->
            boardConfig


updateDatedColumnRangeType : Int -> String -> BoardConfig -> BoardConfig
updateDatedColumnRangeType index rangeType (BoardConfig c) =
    BoardConfig { c | columns = Columns.updateDatedColumnRangeType index rangeType c.columns }


updateDatedColumnRangeValueFrom : Int -> Maybe Int -> BoardConfig -> BoardConfig
updateDatedColumnRangeValueFrom index value ((BoardConfig c) as boardConfig) =
    case value of
        Just newValue ->
            BoardConfig { c | columns = Columns.updateDatedColumnRangeValueFrom index newValue c.columns }

        _ ->
            boardConfig


updateDatedColumnRangeValueTo : Int -> Maybe Int -> BoardConfig -> BoardConfig
updateDatedColumnRangeValueTo index value ((BoardConfig c) as boardConfig) =
    case value of
        Just newValue ->
            BoardConfig { c | columns = Columns.updateDatedColumnRangeValueTo index newValue c.columns }

        _ ->
            boardConfig


updateFilterPolarity : String -> BoardConfig -> BoardConfig
updateFilterPolarity polarity (BoardConfig c) =
    BoardConfig { c | filterPolarity = Filter.polarityFromString polarity }


updateFilterScope : Scope -> BoardConfig -> BoardConfig
updateFilterScope scope (BoardConfig c) =
    BoardConfig { c | filterScope = scope }


updateFilters : List Filter -> BoardConfig -> BoardConfig
updateFilters filters_ (BoardConfig c) =
    BoardConfig { c | filters = filters_ }


updateName : String -> BoardConfig -> BoardConfig
updateName name_ (BoardConfig c) =
    BoardConfig { c | name = name_ }



-- LEGACY


decoder_v_0_10_0 : TsDecode.Decoder BoardConfig
decoder_v_0_10_0 =
    TsDecode.oneOf
        [ DecodeHelpers.toElmVariant "dateBoardConfig"
            identity
            dateBoardConfigDecoder_v_0_10_0
        , DecodeHelpers.toElmVariant "tagBoardConfig"
            identity
            tagBoardConfigDecoder_v_0_10_0
        ]


decoder_v_0_9_0 : TsDecode.Decoder BoardConfig
decoder_v_0_9_0 =
    TsDecode.oneOf
        [ DecodeHelpers.toElmVariant "dateBoardConfig"
            identity
            dateBoardConfigDecoder_v_0_9_0
        , DecodeHelpers.toElmVariant "tagBoardConfig"
            identity
            tagBoardConfigDecoder_v_0_9_0
        ]


decoder_v_0_8_0 : TsDecode.Decoder BoardConfig
decoder_v_0_8_0 =
    TsDecode.oneOf
        [ DecodeHelpers.toElmVariant "dateBoardConfig"
            identity
            dateBoardConfigDecoder_v_0_8_0
        , DecodeHelpers.toElmVariant "tagBoardConfig"
            identity
            tagBoardConfigDecoder_v_0_8_0
        ]


decoder_v_0_7_0 : TsDecode.Decoder BoardConfig
decoder_v_0_7_0 =
    TsDecode.oneOf
        [ DecodeHelpers.toElmVariant "dateBoardConfig"
            identity
            dateBoardConfigDecoder_v_0_7_0
        , DecodeHelpers.toElmVariant "tagBoardConfig"
            identity
            tagBoardConfigDecoder_v_0_7_0
        ]


decoder_v_0_6_0 : TsDecode.Decoder BoardConfig
decoder_v_0_6_0 =
    TsDecode.oneOf
        [ DecodeHelpers.toElmVariant "dateBoardConfig"
            identity
            dateBoardConfigDecoder_v_0_6_0
        , DecodeHelpers.toElmVariant "tagBoardConfig"
            identity
            tagBoardConfigDecoder_v_0_6_0
        ]


decoder_v_0_5_0 : TsDecode.Decoder BoardConfig
decoder_v_0_5_0 =
    TsDecode.oneOf
        [ DecodeHelpers.toElmVariant "dateBoardConfig"
            identity
            dateBoardConfigDecoder_v_0_5_0
        , DecodeHelpers.toElmVariant "tagBoardConfig"
            identity
            tagBoardConfigDecoder_v_0_5_0
        ]


decoder_v_0_4_0 : TsDecode.Decoder BoardConfig
decoder_v_0_4_0 =
    TsDecode.oneOf
        [ DecodeHelpers.toElmVariant "dateBoardConfig"
            identity
            dateBoardConfigDecoder_v_0_4_0
        , DecodeHelpers.toElmVariant "tagBoardConfig"
            identity
            tagBoardConfigDecoder_v_0_4_0
        ]


decoder_v_0_3_0 : TsDecode.Decoder BoardConfig
decoder_v_0_3_0 =
    TsDecode.oneOf
        [ DecodeHelpers.toElmVariant "dateBoardConfig"
            identity
            dateBoardConfigDecoder_v_0_3_0
        , DecodeHelpers.toElmVariant "tagBoardConfig"
            identity
            tagBoardConfigDecoder_v_0_3_0
        ]


decoder_v_0_2_0 : TsDecode.Decoder BoardConfig
decoder_v_0_2_0 =
    TsDecode.oneOf
        [ DecodeHelpers.toElmVariant "dateBoardConfig"
            identity
            dateBoardConfigDecoder_v_0_2_0
        , DecodeHelpers.toElmVariant "tagBoardConfig"
            identity
            tagBoardConfigDecoder_v_0_2_0
        ]


decoder_v_0_1_0 : TsDecode.Decoder BoardConfig
decoder_v_0_1_0 =
    TsDecode.oneOf
        [ DecodeHelpers.toElmVariant "dateBoardConfig"
            identity
            dateBoardConfigDecoder_v_0_1_0
        , DecodeHelpers.toElmVariant "tagBoardConfig"
            identity
            tagBoardConfigDecoder_v_0_1_0
        ]



-- LEGACY DATE BOARD


dateBoardConfigDecoder_v_0_10_0 : TsDecode.Decoder BoardConfig
dateBoardConfigDecoder_v_0_10_0 =
    dateBoardConfigDecoder_v_0_9_0


dateBoardConfigDecoder_v_0_9_0 : TsDecode.Decoder BoardConfig
dateBoardConfigDecoder_v_0_9_0 =
    (TsDecode.succeed buildDateBoardFromPreV11
        |> TsDecode.required "collapsedColumns" (TsDecode.list TsDecode.int)
        |> TsDecode.required "completedCount" TsDecode.int
        |> TsDecode.required "filters" (TsDecode.list Filter.decoder)
        |> TsDecode.required "filterPolarity" Filter.polarityDecoder
        |> TsDecode.required "filterScope" Filter.scopeDecoder
        |> TsDecode.required "includeUndated" TsDecode.bool
        |> TsDecode.required "showFilteredTags" TsDecode.bool
        |> TsDecode.required "title" TsDecode.string
    )
        |> TsDecode.map BoardConfig


dateBoardConfigDecoder_v_0_8_0 : TsDecode.Decoder BoardConfig
dateBoardConfigDecoder_v_0_8_0 =
    (TsDecode.succeed buildDateBoardFromPreV11
        |> TsDecode.hardcoded []
        |> TsDecode.required "completedCount" TsDecode.int
        |> TsDecode.required "filters" (TsDecode.list Filter.decoder)
        |> TsDecode.required "filterPolarity" Filter.polarityDecoder
        |> TsDecode.required "filterScope" Filter.scopeDecoder
        |> TsDecode.required "includeUndated" TsDecode.bool
        |> TsDecode.required "showFilteredTags" TsDecode.bool
        |> TsDecode.required "title" TsDecode.string
    )
        |> TsDecode.map BoardConfig


dateBoardConfigDecoder_v_0_7_0 : TsDecode.Decoder BoardConfig
dateBoardConfigDecoder_v_0_7_0 =
    dateBoardConfigDecoder_v_0_6_0


dateBoardConfigDecoder_v_0_6_0 : TsDecode.Decoder BoardConfig
dateBoardConfigDecoder_v_0_6_0 =
    dateBoardConfigDecoder_v_0_5_0


dateBoardConfigDecoder_v_0_5_0 : TsDecode.Decoder BoardConfig
dateBoardConfigDecoder_v_0_5_0 =
    dateBoardConfigDecoder_v_0_4_0


dateBoardConfigDecoder_v_0_4_0 : TsDecode.Decoder BoardConfig
dateBoardConfigDecoder_v_0_4_0 =
    (TsDecode.succeed buildDateBoardFromPreV11
        |> TsDecode.hardcoded []
        |> TsDecode.required "completedCount" TsDecode.int
        |> TsDecode.required "filters" (TsDecode.list Filter.decoder)
        |> TsDecode.required "filterPolarity" Filter.polarityDecoder
        |> TsDecode.hardcoded Filter.Both
        |> TsDecode.required "includeUndated" TsDecode.bool
        |> TsDecode.required "showFilteredTags" TsDecode.bool
        |> TsDecode.required "title" TsDecode.string
    )
        |> TsDecode.map BoardConfig


dateBoardConfigDecoder_v_0_3_0 : TsDecode.Decoder BoardConfig
dateBoardConfigDecoder_v_0_3_0 =
    (TsDecode.succeed buildDateBoardFromPreV11
        |> TsDecode.hardcoded []
        |> TsDecode.required "completedCount" TsDecode.int
        |> TsDecode.required "filters" (TsDecode.list Filter.decoder)
        |> TsDecode.required "filterPolarity" Filter.polarityDecoder
        |> TsDecode.hardcoded Filter.Both
        |> TsDecode.required "includeUndated" TsDecode.bool
        |> TsDecode.hardcoded True
        |> TsDecode.required "title" TsDecode.string
    )
        |> TsDecode.map BoardConfig


dateBoardConfigDecoder_v_0_2_0 : TsDecode.Decoder BoardConfig
dateBoardConfigDecoder_v_0_2_0 =
    (TsDecode.succeed buildDateBoardFromPreV11
        |> TsDecode.hardcoded []
        |> TsDecode.required "completedCount" TsDecode.int
        |> TsDecode.required "filters" (TsDecode.list Filter.decoder)
        |> TsDecode.hardcoded Filter.Allow
        |> TsDecode.hardcoded Filter.Both
        |> TsDecode.required "includeUndated" TsDecode.bool
        |> TsDecode.hardcoded True
        |> TsDecode.required "title" TsDecode.string
    )
        |> TsDecode.map BoardConfig


dateBoardConfigDecoder_v_0_1_0 : TsDecode.Decoder BoardConfig
dateBoardConfigDecoder_v_0_1_0 =
    (TsDecode.succeed buildDateBoardFromPreV11
        |> TsDecode.hardcoded []
        |> TsDecode.required "completedCount" TsDecode.int
        |> TsDecode.hardcoded []
        |> TsDecode.hardcoded Filter.Allow
        |> TsDecode.hardcoded Filter.Both
        |> TsDecode.required "includeUndated" TsDecode.bool
        |> TsDecode.hardcoded True
        |> TsDecode.required "title" TsDecode.string
    )
        |> TsDecode.map BoardConfig


buildDateBoardFromPreV11 : List Int -> Int -> List Filter -> Polarity -> Scope -> Bool -> Bool -> String -> Config
buildDateBoardFromPreV11 collapsedColumns completedCount_ filters_ filterPolarity_ filterScope_ includeUndated_ showFilteredTags_ title_ =
    let
        columns_ : List Column
        columns_ =
            undatedColumn
                ++ datedColumns
                ++ completedColumn
                |> List.indexedMap handleCollapse

        completedColumn : List Column
        completedColumn =
            if completedCount_ > 0 then
                [ Column.completed <| CompletedColumn.init "Completed" completedColumnIndex completedCount_ ]

            else
                []

        completedColumnIndex : Int
        completedColumnIndex =
            List.length (undatedColumn ++ datedColumns)

        datedColumns : List Column
        datedColumns =
            [ Column.dated <| DatedColumn.init "Today" (DatedColumn.Before 1)
            , Column.dated <| DatedColumn.init "Tomorrow" (DatedColumn.Between { from = 1, to = 1 })
            , Column.dated <| DatedColumn.init "Future" (DatedColumn.After 1)
            ]

        handleCollapse : Int -> Column -> Column
        handleCollapse index column =
            if List.member index collapsedColumns then
                Column.toggleCollapse column

            else
                column

        undatedColumn : List Column
        undatedColumn =
            if includeUndated_ then
                [ Column.undated "Undated" ]

            else
                []
    in
    { columns = Columns.fromList columns_
    , filters = filters_
    , filterPolarity = filterPolarity_
    , filterScope = filterScope_
    , name = title_
    , showFilteredTags = showFilteredTags_
    , showColumnTags = False
    }



-- LEGACY TAG BOARD


tagBoardConfigDecoder_v_0_10_0 : TsDecode.Decoder BoardConfig
tagBoardConfigDecoder_v_0_10_0 =
    tagBoardConfigDecoder_v_0_9_0


tagBoardConfigDecoder_v_0_9_0 : TsDecode.Decoder BoardConfig
tagBoardConfigDecoder_v_0_9_0 =
    (TsDecode.succeed buildTagBoardFromPreV11
        |> TsDecode.required "columns" (TsDecode.list localColumnConfigDecoder)
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
        |> TsDecode.map BoardConfig


tagBoardConfigDecoder_v_0_8_0 : TsDecode.Decoder BoardConfig
tagBoardConfigDecoder_v_0_8_0 =
    (TsDecode.succeed buildTagBoardFromPreV11
        |> TsDecode.required "columns" (TsDecode.list localColumnConfigDecoder)
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
        |> TsDecode.map BoardConfig


tagBoardConfigDecoder_v_0_7_0 : TsDecode.Decoder BoardConfig
tagBoardConfigDecoder_v_0_7_0 =
    tagBoardConfigDecoder_v_0_6_0


tagBoardConfigDecoder_v_0_6_0 : TsDecode.Decoder BoardConfig
tagBoardConfigDecoder_v_0_6_0 =
    tagBoardConfigDecoder_v_0_5_0


tagBoardConfigDecoder_v_0_5_0 : TsDecode.Decoder BoardConfig
tagBoardConfigDecoder_v_0_5_0 =
    tagBoardConfigDecoder_v_0_4_0


tagBoardConfigDecoder_v_0_4_0 : TsDecode.Decoder BoardConfig
tagBoardConfigDecoder_v_0_4_0 =
    (TsDecode.succeed buildTagBoardFromPreV11
        |> TsDecode.required "columns" (TsDecode.list localColumnConfigDecoder)
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
        |> TsDecode.map BoardConfig


tagBoardConfigDecoder_v_0_3_0 : TsDecode.Decoder BoardConfig
tagBoardConfigDecoder_v_0_3_0 =
    (TsDecode.succeed buildTagBoardFromPreV11
        |> TsDecode.required "columns" (TsDecode.list localColumnConfigDecoder)
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
        |> TsDecode.map BoardConfig


tagBoardConfigDecoder_v_0_2_0 : TsDecode.Decoder BoardConfig
tagBoardConfigDecoder_v_0_2_0 =
    (TsDecode.succeed buildTagBoardFromPreV11
        |> TsDecode.required "columns" (TsDecode.list localColumnConfigDecoder)
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
        |> TsDecode.map BoardConfig


tagBoardConfigDecoder_v_0_1_0 : TsDecode.Decoder BoardConfig
tagBoardConfigDecoder_v_0_1_0 =
    (TsDecode.succeed buildTagBoardFromPreV11
        |> TsDecode.required "columns" (TsDecode.list localColumnConfigDecoder)
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
        |> TsDecode.map BoardConfig


buildTagBoardFromPreV11 : List LocalColumnConfig -> List Int -> Int -> List Filter -> Polarity -> Scope -> Bool -> Bool -> Bool -> Bool -> String -> Config
buildTagBoardFromPreV11 localColumnConfigs collapsedColumns completedCount_ filters_ filterPolarity_ filterScope_ includeOthers includeUntagged showColumnTags_ showFilteredTags_ title_ =
    let
        columns_ : List Column
        columns_ =
            untaggedColumn
                ++ otherTagsColumn
                ++ namedTagColumns
                ++ completedColumn
                |> List.indexedMap handleCollapse

        completedColumn : List Column
        completedColumn =
            if completedCount_ > 0 then
                [ Column.completed <| CompletedColumn.init "Completed" completedColumnIndex completedCount_ ]

            else
                []

        completedColumnIndex : Int
        completedColumnIndex =
            List.length (untaggedColumn ++ otherTagsColumn ++ namedTagColumns)

        handleCollapse : Int -> Column -> Column
        handleCollapse index column =
            if List.member index collapsedColumns then
                Column.toggleCollapse column

            else
                column

        namedTagColumns : List Column
        namedTagColumns =
            localColumnConfigs
                |> List.map (\c -> Column.namedTag c.displayName c.tag)

        otherTags : List String
        otherTags =
            localColumnConfigs
                |> List.map .tag

        otherTagsColumn : List Column
        otherTagsColumn =
            if includeOthers then
                [ Column.otherTags "Other Tags" otherTags ]

            else
                []

        untaggedColumn : List Column
        untaggedColumn =
            if includeUntagged then
                [ Column.untagged "Untagged" ]

            else
                []
    in
    { columns = Columns.fromList columns_
    , filters = filters_
    , filterPolarity = filterPolarity_
    , filterScope = filterScope_
    , showColumnTags = showColumnTags_
    , showFilteredTags = showFilteredTags_
    , name = title_
    }


localColumnConfigDecoder : TsDecode.Decoder LocalColumnConfig
localColumnConfigDecoder =
    TsDecode.succeed LocalColumnConfig
        |> TsDecode.andMap (TsDecode.field "tag" TsDecode.string)
        |> TsDecode.andMap (TsDecode.field "displayTitle" TsDecode.string)



-- PRIVATE


config : BoardConfig -> Config
config (BoardConfig c) =
    c


configureOtherTagsColumn : Config -> Config
configureOtherTagsColumn config_ =
    { config_
        | columns =
            Columns.updateOtherTags
                (OtherTagsColumn.setOtherTags <| Columns.namedTagColumnTags config_.columns)
                config_.columns
    }
