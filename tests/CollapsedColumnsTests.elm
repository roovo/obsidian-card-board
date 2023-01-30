module CollapsedColumnsTests exposing (suite)

import CollapsedColumns
import Expect
import Fuzz exposing (Fuzzer)
import Helpers.DecodeHelpers as DecodeHelpers
import Test exposing (..)
import TsJson.Encode as TsEncode


suite : Test
suite =
    concat
        [ columnIsCollapsed
        , decoder
        , encoder
        ]


columnIsCollapsed : Test
columnIsCollapsed =
    describe "columnIsCollapsed"
        [ fuzz (Fuzz.intRange 0 20) "returns False for any column if it has just been initialized" <|
            \fuzzedInt ->
                CollapsedColumns.init
                    |> CollapsedColumns.columnIsCollapsed fuzzedInt
                    |> Expect.equal False
        , fuzz (Fuzz.intRange 0 20) "returns True for any column if it has been set to collapsed" <|
            \fuzzedInt ->
                CollapsedColumns.init
                    |> CollapsedColumns.collapseColumn fuzzedInt True
                    |> CollapsedColumns.columnIsCollapsed fuzzedInt
                    |> Expect.equal True
        , fuzz (Fuzz.intRange 0 20) "returns False for any column if it has been set to not be collapsed" <|
            \fuzzedInt ->
                CollapsedColumns.init
                    |> CollapsedColumns.collapseColumn fuzzedInt True
                    |> CollapsedColumns.collapseColumn fuzzedInt False
                    |> CollapsedColumns.columnIsCollapsed fuzzedInt
                    |> Expect.equal False
        , fuzz (Fuzz.intRange 1 20) "collapsing a column does not affect other columns" <|
            \fuzzedInt ->
                CollapsedColumns.init
                    |> CollapsedColumns.collapseColumn fuzzedInt True
                    |> CollapsedColumns.columnIsCollapsed 0
                    |> Expect.equal False
        ]


decoder : Test
decoder =
    describe "decoder"
        [ test "decodes an empty array" <|
            \() ->
                "[]"
                    |> DecodeHelpers.runDecoder CollapsedColumns.decoder
                    |> .decoded
                    |> Expect.equal (Ok CollapsedColumns.init)
        , test "decodes '[1,3,4]'" <|
            \() ->
                "[1,3,4]"
                    |> DecodeHelpers.runDecoder CollapsedColumns.decoder
                    |> .decoded
                    |> Expect.equal
                        (Ok
                            (CollapsedColumns.init
                                |> CollapsedColumns.collapseColumn 1 True
                                |> CollapsedColumns.collapseColumn 3 True
                                |> CollapsedColumns.collapseColumn 4 True
                            )
                        )
        ]


encoder : Test
encoder =
    describe "encoder"
        [ test "encodes an initialized CollapsedColumns" <|
            \() ->
                CollapsedColumns.init
                    |> TsEncode.runExample CollapsedColumns.encoder
                    |> .output
                    |> Expect.equal "[]"
        , test "encodes an CollapsedColumns with some collapsed columns" <|
            \() ->
                (CollapsedColumns.init
                    |> CollapsedColumns.collapseColumn 1 True
                    |> CollapsedColumns.collapseColumn 3 True
                    |> CollapsedColumns.collapseColumn 4 True
                )
                    |> TsEncode.runExample CollapsedColumns.encoder
                    |> .output
                    |> Expect.equal "[1,3,4]"
        ]
