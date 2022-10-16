module GlobalSettingsTests exposing (suite)

import BoardConfig
import Expect
import GlobalSettings exposing (GlobalSettings)
import Helpers.BoardConfigHelpers as BoardConfigHelpers
import Helpers.DecodeHelpers as DecodeHelpers
import Semver
import Test exposing (..)
import TsJson.Encode as TsEncode


suite : Test
suite =
    concat
        [ updateTaskUpdateFormat
        ]


updateTaskUpdateFormat : Test
updateTaskUpdateFormat =
    describe "updateTaskUpdateFormat"
        [ test "can update to be ObsidianCardBoard format" <|
            \() ->
                exampleGlobalSettings
                    |> GlobalSettings.updateTaskUpdateFormat "ObsidianCardBoard"
                    |> Expect.equal { taskUpdateFormat = GlobalSettings.ObsidianCardBoard }
        , test "can update to be ObsidianTasks format" <|
            \() ->
                exampleGlobalSettings
                    |> GlobalSettings.updateTaskUpdateFormat "ObsidianCardBoard"
                    |> GlobalSettings.updateTaskUpdateFormat "ObsidianTasks"
                    |> Expect.equal { taskUpdateFormat = GlobalSettings.ObsidianTasks }
        , test "defaults to be ObsidianCardBoard if the string is not recognised" <|
            \() ->
                exampleGlobalSettings
                    |> GlobalSettings.updateTaskUpdateFormat "ObsidianCardBoard"
                    |> GlobalSettings.updateTaskUpdateFormat "ObsidianTasks"
                    |> GlobalSettings.updateTaskUpdateFormat "xxxxxx"
                    |> Expect.equal { taskUpdateFormat = GlobalSettings.ObsidianCardBoard }
        ]



-- HELPERS


exampleGlobalSettings : GlobalSettings
exampleGlobalSettings =
    { taskUpdateFormat = GlobalSettings.ObsidianTasks }
