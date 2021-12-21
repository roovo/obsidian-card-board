module BoardConfigTests exposing (suite)

import BoardConfig exposing (BoardConfig)
import DateBoard
import Expect
import Filter
import Helpers.BoardConfigHelpers as BoardConfigHelpers
import Helpers.DecodeHelpers as DecodeHelpers
import Helpers.FilterHelpers as FilterHelpers
import TagBoard
import Test exposing (..)
import TsJson.Encode as TsEncode


suite : Test
suite =
    concat
        [ default
        , encodeDecode
        , fromBoardType
        , mapFilters
        , toggleIncludeOthers
        , toggleIncludeUndated
        , toggleIncludeUntagged
        , updateBoardType
        , updateCompletedCount
        , updateFilters
        , updateTags
        , updateTitle
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
                    |> DecodeHelpers.runDecoder BoardConfig.decoder
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


toggleIncludeOthers : Test
toggleIncludeOthers =
    describe "toggleIncludeOthers"
        [ test "does nothing to a DateBoard config" <|
            \() ->
                BoardConfig.fromBoardType "dateBoard" ""
                    |> BoardConfig.toggleIncludeOthers
                    |> extractDateBoardConfig
                    |> Expect.equal (Just DateBoard.defaultConfig)
        , test "toggles includeOthers for a TagBoard config" <|
            \() ->
                BoardConfig.fromBoardType "tagBoard" ""
                    |> BoardConfig.toggleIncludeOthers
                    |> extractTagBoardConfig
                    |> Maybe.map .includeOthers
                    |> Expect.equal (Just <| not defaultTagBoardConfig.includeOthers)
        ]


toggleIncludeUndated : Test
toggleIncludeUndated =
    describe "toggleIncludeUndated"
        [ test "toggled includeUndated for a DateBoard config" <|
            \() ->
                BoardConfig.fromBoardType "dateBoard" ""
                    |> BoardConfig.toggleIncludeUndated
                    |> extractDateBoardConfig
                    |> Maybe.map .includeUndated
                    |> Expect.equal (Just <| not defaultDateBoardConfig.includeUndated)
        , test "does nothing for a TagBoard config" <|
            \() ->
                BoardConfig.fromBoardType "tagBoard" ""
                    |> BoardConfig.toggleIncludeUndated
                    |> extractTagBoardConfig
                    |> Expect.equal (Just TagBoard.defaultConfig)
        ]


toggleIncludeUntagged : Test
toggleIncludeUntagged =
    describe "toggleIncludeUntagged"
        [ test "does nothing to a DateBoard config" <|
            \() ->
                BoardConfig.fromBoardType "dateBoard" ""
                    |> BoardConfig.toggleIncludeUntagged
                    |> extractDateBoardConfig
                    |> Expect.equal (Just DateBoard.defaultConfig)
        , test "toggles includeUntagged for a TagBoard config" <|
            \() ->
                BoardConfig.fromBoardType "tagBoard" ""
                    |> BoardConfig.toggleIncludeUntagged
                    |> extractTagBoardConfig
                    |> Maybe.map .includeUntagged
                    |> Expect.equal (Just <| not defaultTagBoardConfig.includeUntagged)
        ]


updateBoardType : Test
updateBoardType =
    describe "updateBoardType"
        [ test "builds a new DateBoard config from a TagBoard config" <|
            \() ->
                BoardConfig.fromBoardType "tagBoard" "some title"
                    |> BoardConfig.updateBoardType "dateBoard"
                    |> BoardConfig.isForDateBoard
                    |> Expect.equal True
        , test "keeps the title when converting a TagBoard into a DateBoard config" <|
            \() ->
                BoardConfig.fromBoardType "tagBoard" "some title"
                    |> BoardConfig.updateBoardType "dateBoard"
                    |> BoardConfig.title
                    |> Expect.equal "some title"
        , test "builds a new TagBoard config from a DateBoard config" <|
            \() ->
                BoardConfig.fromBoardType "dateBoard" "some title"
                    |> BoardConfig.updateBoardType "tagBoard"
                    |> BoardConfig.isForTagBoard
                    |> Expect.equal True
        , test "keeps the title when converting a DateBoard into a TagBoard config" <|
            \() ->
                BoardConfig.fromBoardType "dateBoard" "some title"
                    |> BoardConfig.updateBoardType "tagBoard"
                    |> BoardConfig.title
                    |> Expect.equal "some title"
        ]


updateCompletedCount : Test
updateCompletedCount =
    describe "updateCompletedCount"
        [ test "updates the completedCount for a DateBoard config" <|
            \() ->
                BoardConfig.fromBoardType "dateBoard" ""
                    |> BoardConfig.updateCompletedCount (Just 33)
                    |> extractDateBoardConfig
                    |> Maybe.map .completedCount
                    |> Expect.equal (Just 33)
        , test "updates the completedCount for a TagBoard config" <|
            \() ->
                BoardConfig.fromBoardType "tagBoard" ""
                    |> BoardConfig.updateCompletedCount (Just 33)
                    |> extractTagBoardConfig
                    |> Maybe.map .completedCount
                    |> Expect.equal (Just 33)
        ]


updateFilters : Test
updateFilters =
    describe "updateFilters"
        [ test "updates the filters for a DateBoard config" <|
            \() ->
                BoardConfig.fromBoardType "dateBoard" "a title"
                    |> BoardConfig.updateFilters FilterHelpers.exampleFilters
                    |> BoardConfig.filters
                    |> Expect.equal FilterHelpers.exampleFilters
        , test "updates the filters for a TagBoard config" <|
            \() ->
                BoardConfig.fromBoardType "tagBoard" "a title"
                    |> BoardConfig.updateFilters FilterHelpers.exampleFilters
                    |> BoardConfig.filters
                    |> Expect.equal FilterHelpers.exampleFilters
        ]


updateTags : Test
updateTags =
    describe "updateTags"
        [ test "does nothing to a DateBoard config" <|
            \() ->
                BoardConfig.fromBoardType "dateBoard" ""
                    |> BoardConfig.updateTags ""
                    |> extractDateBoardConfig
                    |> Expect.equal (Just DateBoard.defaultConfig)
        , test "updates the tags for a TagBoard config" <|
            \() ->
                BoardConfig.fromBoardType "tagBoard" ""
                    |> BoardConfig.updateTags "bar"
                    |> BoardConfig.updateTags "foo"
                    |> extractTagBoardConfig
                    |> Maybe.map .columns
                    |> Expect.equal (Just [ { tag = "foo", displayTitle = "Foo" } ])
        ]


updateTitle : Test
updateTitle =
    describe "updateTitle"
        [ test "updates the title for a DateBoard config" <|
            \() ->
                BoardConfig.fromBoardType "dateBoard" "a title"
                    |> BoardConfig.updateTitle "new title"
                    |> BoardConfig.title
                    |> Expect.equal "new title"
        , test "updates the title for a TagBoard config" <|
            \() ->
                BoardConfig.fromBoardType "tagBoard" "a title"
                    |> BoardConfig.updateTitle "new title"
                    |> BoardConfig.title
                    |> Expect.equal "new title"
        ]



-- HELPERS


defaultDateBoardConfig : DateBoard.Config
defaultDateBoardConfig =
    DateBoard.defaultConfig


defaultTagBoardConfig : TagBoard.Config
defaultTagBoardConfig =
    TagBoard.defaultConfig


extractDateBoardConfig : BoardConfig -> Maybe DateBoard.Config
extractDateBoardConfig boardConfig =
    case boardConfig of
        BoardConfig.DateBoardConfig config ->
            Just config

        _ ->
            Nothing


extractTagBoardConfig : BoardConfig -> Maybe TagBoard.Config
extractTagBoardConfig boardConfig =
    case boardConfig of
        BoardConfig.TagBoardConfig config ->
            Just config

        _ ->
            Nothing
