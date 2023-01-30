module CollapseStatesTests exposing (suite)

import CollapseStates
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
                CollapseStates.init
                    |> CollapseStates.columnIsCollapsed fuzzedInt
                    |> Expect.equal False
        , fuzz (Fuzz.intRange 0 20) "returns True for any column if it has been set to collapsed" <|
            \fuzzedInt ->
                CollapseStates.init
                    |> CollapseStates.collapseColumn fuzzedInt True
                    |> CollapseStates.columnIsCollapsed fuzzedInt
                    |> Expect.equal True
        , fuzz (Fuzz.intRange 0 20) "returns False for any column if it has been set to not be collapsed" <|
            \fuzzedInt ->
                CollapseStates.init
                    |> CollapseStates.collapseColumn fuzzedInt True
                    |> CollapseStates.collapseColumn fuzzedInt False
                    |> CollapseStates.columnIsCollapsed fuzzedInt
                    |> Expect.equal False
        , fuzz (Fuzz.intRange 1 20) "collapsing a column does not affect other columns" <|
            \fuzzedInt ->
                CollapseStates.init
                    |> CollapseStates.collapseColumn fuzzedInt True
                    |> CollapseStates.columnIsCollapsed 0
                    |> Expect.equal False
        ]


decoder : Test
decoder =
    describe "decoder"
        [ test "decodes an empty array" <|
            \() ->
                "[]"
                    |> DecodeHelpers.runDecoder CollapseStates.decoder
                    |> .decoded
                    |> Expect.equal (Ok CollapseStates.init)
        , test "decodes '[1,3,4]'" <|
            \() ->
                "[1,3,4]"
                    |> DecodeHelpers.runDecoder CollapseStates.decoder
                    |> .decoded
                    |> Expect.equal
                        (Ok
                            (CollapseStates.init
                                |> CollapseStates.collapseColumn 1 True
                                |> CollapseStates.collapseColumn 3 True
                                |> CollapseStates.collapseColumn 4 True
                            )
                        )
        ]


encoder : Test
encoder =
    describe "encoder"
        [ test "encodes an initialized CollapseStates" <|
            \() ->
                CollapseStates.init
                    |> TsEncode.runExample CollapseStates.encoder
                    |> .output
                    |> Expect.equal "[]"
        , test "encodes an CollapseStates with some collapsed columns" <|
            \() ->
                (CollapseStates.init
                    |> CollapseStates.collapseColumn 1 True
                    |> CollapseStates.collapseColumn 3 True
                    |> CollapseStates.collapseColumn 4 True
                )
                    |> TsEncode.runExample CollapseStates.encoder
                    |> .output
                    |> Expect.equal "[1,3,4]"
        ]
