module BoardConfigTests exposing (suite)

import BoardConfig exposing (BoardConfig)
import Column
import Columns
import DateBoardConfig exposing (DateBoardConfig)
import Expect
import Filter
import Helpers.BoardConfigHelpers as BoardConfigHelpers
import Helpers.DecodeHelpers as DecodeHelpers
import Helpers.FilterHelpers as FilterHelpers
import TagBoardConfig exposing (TagBoardConfig)
import Test exposing (..)
import TsJson.Encode as TsEncode


suite : Test
suite =
    concat
        [ default
        , encodeDecode
        , fromBoardType
        , isForDateBoard
        , isForTagBoard
        , mapFilters
        , collapseColumn

        -- , toggleIncludeOthers
        -- , toggleIncludeUndated
        -- , toggleIncludeUntagged
        -- , toggleShowColumnTags
        -- , toggleShowFilteredTags
        -- , toggleTagFilterScope
        -- , updateBoardType
        -- , updateCompletedCount
        -- , updateFilters
        -- , updateFilterPolarity
        -- , updateTags
        -- , updateTitle
        ]


default : Test
default =
    describe "default"
        [ test "is for a TagBoard" <|
            \() ->
                BoardConfig.default
                    |> BoardConfig.isForTagBoard
                    |> Expect.equal True
        ]


encodeDecode : Test
encodeDecode =
    describe "encoding and decoding config"
        [ test "can decode an encoded string back to the original" <|
            \() ->
                BoardConfigHelpers.exampleBoardConfig
                    |> TsEncode.runExample BoardConfig.encoder
                    |> .output
                    |> DecodeHelpers.runDecoder BoardConfig.decoder_v_0_11_0
                    |> .decoded
                    |> Expect.equal (Ok BoardConfigHelpers.exampleBoardConfig)
        ]


fromBoardType : Test
fromBoardType =
    describe "fromBoardType"
        [ test "builds a DateBoard config" <|
            \() ->
                BoardConfig.fromBoardType "dateBoard" "some title"
                    |> BoardConfig.isForDateBoard
                    |> Expect.equal True
        , test "sets the title of the DateBoard config" <|
            \() ->
                BoardConfig.fromBoardType "dateBoard" "some title"
                    |> BoardConfig.title
                    |> Expect.equal "some title"
        , test "builds a TagBoard config" <|
            \() ->
                BoardConfig.fromBoardType "NOT dateBoard" "some title"
                    |> BoardConfig.isForTagBoard
                    |> Expect.equal True
        , test "sets the title of the TagBoard config" <|
            \() ->
                BoardConfig.fromBoardType "NOT dateBoard" "some title"
                    |> BoardConfig.title
                    |> Expect.equal "some title"
        ]


isForDateBoard : Test
isForDateBoard =
    describe "isForDateBoard"
        [ test "returns True for a DateBoard" <|
            \() ->
                BoardConfig.fromBoardType "dateBoard" "some title"
                    |> BoardConfig.isForDateBoard
                    |> Expect.equal True
        , test "returns False for a TagBoard" <|
            \() ->
                BoardConfig.fromBoardType "tagBoard" "some title"
                    |> BoardConfig.isForDateBoard
                    |> Expect.equal False
        ]


isForTagBoard : Test
isForTagBoard =
    describe "isForTagBoard"
        [ test "returns True for a TagBoard" <|
            \() ->
                BoardConfig.fromBoardType "tagBoard" "some title"
                    |> BoardConfig.isForTagBoard
                    |> Expect.equal True
        , test "returns False for a TagBoard" <|
            \() ->
                BoardConfig.fromBoardType "dateBoard" "some title"
                    |> BoardConfig.isForTagBoard
                    |> Expect.equal False
        ]


mapFilters : Test
mapFilters =
    describe "mapFilters"
        [ test "updates the filters for a DateBoard config" <|
            \() ->
                BoardConfig.fromBoardType "dateBoard" "a title"
                    |> BoardConfig.updateFilters FilterHelpers.exampleFilters
                    |> BoardConfig.mapFilters (always Filter.dummy)
                    |> BoardConfig.filters
                    |> List.map Filter.value
                    |> Expect.equal [ "", "", "" ]
        , test "updates the filters for a TagBoard config" <|
            \() ->
                BoardConfig.fromBoardType "tagBoard" "a title"
                    |> BoardConfig.updateFilters FilterHelpers.exampleFilters
                    |> BoardConfig.mapFilters (always Filter.dummy)
                    |> BoardConfig.filters
                    |> List.map Filter.value
                    |> Expect.equal [ "", "", "" ]
        ]


collapseColumn : Test
collapseColumn =
    describe "collapseColumn"
        [ test "collapses a column for a DateBoard config" <|
            \() ->
                BoardConfig.fromBoardType "dateBoard" ""
                    |> BoardConfig.collapseColumn 1 True
                    |> extractDateBoardConfig
                    |> Maybe.map .columns
                    |> Maybe.map Columns.toList
                    |> Maybe.withDefault []
                    |> List.map Column.isCollapsed
                    |> Expect.equal [ False, True, False, False, False ]
        , test "uncollapses a column for a DateBoard config" <|
            \() ->
                BoardConfig.fromBoardType "dateBoard" ""
                    |> BoardConfig.collapseColumn 1 True
                    |> BoardConfig.collapseColumn 1 False
                    |> extractDateBoardConfig
                    |> Maybe.map .columns
                    |> Maybe.map Columns.toList
                    |> Maybe.withDefault []
                    |> List.map Column.isCollapsed
                    |> Expect.equal [ False, False, False, False, False ]
        ]



--
--
-- toggleIncludeOthers : Test
-- toggleIncludeOthers =
--     describe "toggleIncludeOthers"
--         [ test "does nothing to a DateBoard config" <|
--             \() ->
--                 BoardConfig.fromBoardType "dateBoard" ""
--                     |> BoardConfig.toggleIncludeOthers
--                     |> extractDateBoardConfig
--                     |> Expect.equal (Just DateBoardConfig.default)
--         , test "toggles includeOthers for a TagBoard config" <|
--             \() ->
--                 BoardConfig.fromBoardType "tagBoard" ""
--                     |> BoardConfig.toggleIncludeOthers
--                     |> extractTagBoardConfig
--                     |> Maybe.map .includeOthers
--                     |> Expect.equal (Just <| not defaultTagBoardConfig.includeOthers)
--         ]
--
--
-- toggleIncludeUndated : Test
-- toggleIncludeUndated =
--     describe "toggleIncludeUndated"
--         [ test "toggled includeUndated for a DateBoard config" <|
--             \() ->
--                 BoardConfig.fromBoardType "dateBoard" ""
--                     |> BoardConfig.toggleIncludeUndated
--                     |> extractDateBoardConfig
--                     |> Maybe.map .includeUndated
--                     |> Expect.equal (Just <| not defaultDateBoardConfig.includeUndated)
--         , test "does nothing for a TagBoard config" <|
--             \() ->
--                 BoardConfig.fromBoardType "tagBoard" ""
--                     |> BoardConfig.toggleIncludeUndated
--                     |> extractTagBoardConfig
--                     |> Expect.equal (Just TagBoardConfig.default)
--         ]
--
--
-- toggleIncludeUntagged : Test
-- toggleIncludeUntagged =
--     describe "toggleIncludeUntagged"
--         [ test "does nothing to a DateBoard config" <|
--             \() ->
--                 BoardConfig.fromBoardType "dateBoard" ""
--                     |> BoardConfig.toggleIncludeUntagged
--                     |> extractDateBoardConfig
--                     |> Expect.equal (Just DateBoardConfig.default)
--         , test "toggles includeUntagged for a TagBoard config" <|
--             \() ->
--                 BoardConfig.fromBoardType "tagBoard" ""
--                     |> BoardConfig.toggleIncludeUntagged
--                     |> extractTagBoardConfig
--                     |> Maybe.map .includeUntagged
--                     |> Expect.equal (Just <| not defaultTagBoardConfig.includeUntagged)
--         ]
--
--
-- toggleShowColumnTags : Test
-- toggleShowColumnTags =
--     describe "toggleShowColumnTags"
--         [ test "does nothing to a DateBoard config" <|
--             \() ->
--                 BoardConfig.fromBoardType "dateBoard" ""
--                     |> BoardConfig.toggleShowColumnTags
--                     |> extractDateBoardConfig
--                     |> Expect.equal (Just DateBoardConfig.default)
--         , test "toggles includeUntagged for a TagBoard config" <|
--             \() ->
--                 BoardConfig.fromBoardType "tagBoard" ""
--                     |> BoardConfig.toggleShowColumnTags
--                     |> extractTagBoardConfig
--                     |> Maybe.map .showColumnTags
--                     |> Expect.equal (Just <| not defaultTagBoardConfig.showColumnTags)
--         ]
--
--
-- toggleShowFilteredTags : Test
-- toggleShowFilteredTags =
--     describe "toggleShowFilteredTags"
--         [ test "toggles includeUntagged for a DateBoard config" <|
--             \() ->
--                 BoardConfig.fromBoardType "dateBoard" ""
--                     |> BoardConfig.toggleShowFilteredTags
--                     |> extractDateBoardConfig
--                     |> Maybe.map .showFilteredTags
--                     |> Expect.equal (Just <| not defaultTagBoardConfig.showFilteredTags)
--         , test "toggles includeUntagged for a TagBoard config" <|
--             \() ->
--                 BoardConfig.fromBoardType "tagBoard" ""
--                     |> BoardConfig.toggleShowFilteredTags
--                     |> extractTagBoardConfig
--                     |> Maybe.map .showFilteredTags
--                     |> Expect.equal (Just <| not defaultTagBoardConfig.showFilteredTags)
--         ]
--
--
-- toggleTagFilterScope : Test
-- toggleTagFilterScope =
--     describe "toggleTagFilterScope"
--         [ test "toggles TopLevelOnly -> SubTasksOnly for a DateBoard config" <|
--             \() ->
--                 DateBoardConfig.default
--                     |> (\c -> { c | filterScope = Filter.TopLevelOnly })
--                     |> BoardConfig.DateBoardConfig
--                     |> BoardConfig.toggleTagFilterScope
--                     |> extractDateBoardConfig
--                     |> Maybe.map .filterScope
--                     |> Expect.equal (Just Filter.SubTasksOnly)
--         , test "toggles SubTasksOnly -> Both for a DateBoard config" <|
--             \() ->
--                 DateBoardConfig.default
--                     |> (\c -> { c | filterScope = Filter.SubTasksOnly })
--                     |> BoardConfig.DateBoardConfig
--                     |> BoardConfig.toggleTagFilterScope
--                     |> extractDateBoardConfig
--                     |> Maybe.map .filterScope
--                     |> Expect.equal (Just Filter.Both)
--         , test "toggles Both -> TopLevelOnly for a DateBoard config" <|
--             \() ->
--                 DateBoardConfig.default
--                     |> (\c -> { c | filterScope = Filter.Both })
--                     |> BoardConfig.DateBoardConfig
--                     |> BoardConfig.toggleTagFilterScope
--                     |> extractDateBoardConfig
--                     |> Maybe.map .filterScope
--                     |> Expect.equal (Just Filter.TopLevelOnly)
--         , test "toggles TopLevelOnly -> SubTasksOnly for a TagBoard config" <|
--             \() ->
--                 TagBoardConfig.default
--                     |> (\c -> { c | filterScope = Filter.TopLevelOnly })
--                     |> BoardConfig.TagBoardConfig
--                     |> BoardConfig.toggleTagFilterScope
--                     |> extractTagBoardConfig
--                     |> Maybe.map .filterScope
--                     |> Expect.equal (Just Filter.SubTasksOnly)
--         , test "toggles SubTasksOnly -> Both for a TagBoard config" <|
--             \() ->
--                 TagBoardConfig.default
--                     |> (\c -> { c | filterScope = Filter.SubTasksOnly })
--                     |> BoardConfig.TagBoardConfig
--                     |> BoardConfig.toggleTagFilterScope
--                     |> extractTagBoardConfig
--                     |> Maybe.map .filterScope
--                     |> Expect.equal (Just Filter.Both)
--         , test "toggles Both -> TopLevelOnly for a TagBoard config" <|
--             \() ->
--                 TagBoardConfig.default
--                     |> (\c -> { c | filterScope = Filter.Both })
--                     |> BoardConfig.TagBoardConfig
--                     |> BoardConfig.toggleTagFilterScope
--                     |> extractTagBoardConfig
--                     |> Maybe.map .filterScope
--                     |> Expect.equal (Just Filter.TopLevelOnly)
--         ]
--
--
-- updateBoardType : Test
-- updateBoardType =
--     describe "updateBoardType"
--         [ test "builds a new DateBoard config from a TagBoard config" <|
--             \() ->
--                 BoardConfig.fromBoardType "tagBoard" "some title"
--                     |> BoardConfig.updateBoardType "dateBoard"
--                     |> BoardConfig.isForDateBoard
--                     |> Expect.equal True
--         , test "keeps the title when converting a TagBoard into a DateBoard config" <|
--             \() ->
--                 BoardConfig.fromBoardType "tagBoard" "some title"
--                     |> BoardConfig.updateBoardType "dateBoard"
--                     |> BoardConfig.title
--                     |> Expect.equal "some title"
--         , test "builds a new TagBoard config from a DateBoard config" <|
--             \() ->
--                 BoardConfig.fromBoardType "dateBoard" "some title"
--                     |> BoardConfig.updateBoardType "tagBoard"
--                     |> BoardConfig.isForTagBoard
--                     |> Expect.equal True
--         , test "keeps the title when converting a DateBoard into a TagBoard config" <|
--             \() ->
--                 BoardConfig.fromBoardType "dateBoard" "some title"
--                     |> BoardConfig.updateBoardType "tagBoard"
--                     |> BoardConfig.title
--                     |> Expect.equal "some title"
--         ]
--
--
-- updateCompletedCount : Test
-- updateCompletedCount =
--     describe "updateCompletedCount"
--         [ test "updates the completedCount for a DateBoard config" <|
--             \() ->
--                 BoardConfig.fromBoardType "dateBoard" ""
--                     |> BoardConfig.updateCompletedCount (Just 33)
--                     |> extractDateBoardConfig
--                     |> Maybe.map .completedCount
--                     |> Expect.equal (Just 33)
--         , test "updates the completedCount for a TagBoard config" <|
--             \() ->
--                 BoardConfig.fromBoardType "tagBoard" ""
--                     |> BoardConfig.updateCompletedCount (Just 33)
--                     |> extractTagBoardConfig
--                     |> Maybe.map .completedCount
--                     |> Expect.equal (Just 33)
--         , test "does NOT update the completedCount for a DateBoard config if the count is Nothing" <|
--             \() ->
--                 BoardConfig.fromBoardType "dateBoard" ""
--                     |> BoardConfig.updateCompletedCount Nothing
--                     |> extractDateBoardConfig
--                     |> Maybe.map .completedCount
--                     |> Expect.equal (Just 10)
--         , test "does NOT update the completedCount for a TagBoard config if the count is Nothing" <|
--             \() ->
--                 BoardConfig.fromBoardType "tagBoard" ""
--                     |> BoardConfig.updateCompletedCount Nothing
--                     |> extractTagBoardConfig
--                     |> Maybe.map .completedCount
--                     |> Expect.equal (Just 10)
--         ]
--
--
-- updateFilters : Test
-- updateFilters =
--     describe "updateFilters"
--         [ test "updates the filters for a DateBoard config" <|
--             \() ->
--                 BoardConfig.fromBoardType "dateBoard" "a title"
--                     |> BoardConfig.updateFilters FilterHelpers.exampleFilters
--                     |> BoardConfig.filters
--                     |> Expect.equal FilterHelpers.exampleFilters
--         , test "updates the filters for a TagBoard config" <|
--             \() ->
--                 BoardConfig.fromBoardType "tagBoard" "a title"
--                     |> BoardConfig.updateFilters FilterHelpers.exampleFilters
--                     |> BoardConfig.filters
--                     |> Expect.equal FilterHelpers.exampleFilters
--         ]
--
--
-- updateFilterPolarity : Test
-- updateFilterPolarity =
--     describe "updateFilterPolarity"
--         [ test "updates the filter polarity for a DateBoard config" <|
--             \() ->
--                 BoardConfig.fromBoardType "dateBoard" "a title"
--                     |> BoardConfig.updateFilterPolarity "Deny"
--                     |> BoardConfig.filterPolarity
--                     |> Expect.equal Filter.Deny
--         , test "updates the filter polarity for a TagBoard config" <|
--             \() ->
--                 BoardConfig.fromBoardType "tagBoard" "a title"
--                     |> BoardConfig.updateFilterPolarity "Deny"
--                     |> BoardConfig.filterPolarity
--                     |> Expect.equal Filter.Deny
--         ]
--
--
-- updateTags : Test
-- updateTags =
--     describe "updateTags"
--         [ test "does nothing to a DateBoard config" <|
--             \() ->
--                 BoardConfig.fromBoardType "dateBoard" ""
--                     |> BoardConfig.updateTags ""
--                     |> extractDateBoardConfig
--                     |> Expect.equal (Just DateBoardConfig.default)
--         , test "updates the tags for a TagBoard config" <|
--             \() ->
--                 BoardConfig.fromBoardType "tagBoard" ""
--                     |> BoardConfig.updateTags "bar"
--                     |> BoardConfig.updateTags "foo"
--                     |> extractTagBoardConfig
--                     |> Maybe.map .columns
--                     |> Expect.equal (Just [ { tag = "foo", displayTitle = "Foo" } ])
--         ]
--
--
-- updateTitle : Test
-- updateTitle =
--     describe "updateTitle"
--         [ test "updates the title for a DateBoard config" <|
--             \() ->
--                 BoardConfig.fromBoardType "dateBoard" "a title"
--                     |> BoardConfig.updateTitle "new title"
--                     |> BoardConfig.title
--                     |> Expect.equal "new title"
--         , test "updates the title for a TagBoard config" <|
--             \() ->
--                 BoardConfig.fromBoardType "tagBoard" "a title"
--                     |> BoardConfig.updateTitle "new title"
--                     |> BoardConfig.title
--                     |> Expect.equal "new title"
--         ]
--
--
--
-- -- HELPERS
--
--
-- defaultDateBoardConfig : DateBoardConfig
-- defaultDateBoardConfig =
--     DateBoardConfig.default
--
--
-- defaultTagBoardConfig : TagBoardConfig
-- defaultTagBoardConfig =
--     TagBoardConfig.default


extractDateBoardConfig : BoardConfig -> Maybe DateBoardConfig
extractDateBoardConfig boardConfig =
    case boardConfig of
        BoardConfig.DateBoardConfig config ->
            Just config

        _ ->
            Nothing


extractTagBoardConfig : BoardConfig -> Maybe TagBoardConfig
extractTagBoardConfig boardConfig =
    case boardConfig of
        BoardConfig.TagBoardConfig config ->
            Just config

        _ ->
            Nothing
