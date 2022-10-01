module TagListTests exposing (suite)

import Expect
import TagList
import Test exposing (..)


suite : Test
suite =
    concat
        [ empty
        ]


empty : Test
empty =
    describe "empty"
        [ test "contains no Tags" <|
            \() ->
                TagList.empty
                    |> TagList.toString
                    |> Expect.equal ""
        ]
