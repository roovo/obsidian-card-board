module TodoTests exposing (suite)

import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    describe "just testing"
        [ test "true is true" <|
            \() ->
                True
                    |> Expect.equal True
        ]
