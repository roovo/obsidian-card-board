module CollapseStatesTests exposing (suite)

import CollapseStates
import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (..)


suite : Test
suite =
    concat
        [ columnIsCollapsed
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
