module BoardTests exposing (suite)

-- import DateBoard
-- import Helpers.BoardHelpers as BoardHelpers
-- import Helpers.DateTimeHelpers as DateTimeHelpers
-- import Helpers.DecodeHelpers as DecodeHelpers
-- import TaskItem exposing (TaskItem)
-- import TsJson.Encode as TsEncode

import Expect
import Test exposing (..)


suite : Test
suite =
    concat
        [ columns
        ]



-- init : BoardConfig -> TaskList -> Board
-- init config taskList =
--     Board config taskList
--
--
--
-- -- INFO
--
--
-- columns : TimeWithZone -> Int -> Board -> List ( String, List Card )


columns : Test
columns =
    describe "columns"
        [ test "default columns are just today tomorrow and future" <|
            \() ->
                True
                    |> Expect.equal True
        ]
