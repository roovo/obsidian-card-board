module CollapseStatesTests exposing (suite)

import CollapseStates
import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (..)


suite : Test
suite =
    concat
        [ columnIsToggled
        ]


columnIsToggled : Test
columnIsToggled =
    describe "isToggled"
        [ fuzz (Fuzz.intRange 0 20) "returns False for any column if nothing has been toggled" <|
            \fuzzedInt ->
                CollapseStates.init
                    |> CollapseStates.columnIsToggled fuzzedInt
                    |> Expect.equal False
        , fuzz (Fuzz.intRange 0 20) "returns True for any column if it has been toggled" <|
            \fuzzedInt ->
                CollapseStates.init
                    |> CollapseStates.toggleColumn fuzzedInt
                    |> CollapseStates.columnIsToggled fuzzedInt
                    |> Expect.equal True
        , fuzz (Fuzz.intRange 0 20) "returns False for any column if it has been toggled twice" <|
            \fuzzedInt ->
                CollapseStates.init
                    |> CollapseStates.toggleColumn fuzzedInt
                    |> CollapseStates.toggleColumn fuzzedInt
                    |> CollapseStates.columnIsToggled fuzzedInt
                    |> Expect.equal False
        , fuzz (Fuzz.intRange 1 20) "toggling a column does not affect other columns" <|
            \fuzzedInt ->
                CollapseStates.init
                    |> CollapseStates.toggleColumn fuzzedInt
                    |> CollapseStates.columnIsToggled 0
                    |> Expect.equal False
        ]
