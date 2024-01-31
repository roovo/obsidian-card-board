module DueDateTests exposing (suite)

import Date
import DueDate
import Expect
import Test exposing (..)
import TsJson.Encode as TsEncode


suite : Test
suite =
    concat
        [ encoder
        ]


encoder : Test
encoder =
    describe "encoder"
        [ test "encodes a NotSet" <|
            \() ->
                DueDate.NotSet
                    |> TsEncode.runExample DueDate.encoder
                    |> .output
                    |> Expect.equal "{\"tag\":\"NotSet\"}"
        , test "encodes a SetToDate" <|
            \() ->
                DueDate.SetToDate (Date.fromRataDie 123123)
                    |> TsEncode.runExample DueDate.encoder
                    |> .output
                    |> Expect.equal "{\"tag\":\"SetToDate\",\"date\":123123}"
        , test "encodes a SetToNone" <|
            \() ->
                DueDate.SetToNone
                    |> TsEncode.runExample DueDate.encoder
                    |> .output
                    |> Expect.equal "{\"tag\":\"SetToNone\"}"
        ]
