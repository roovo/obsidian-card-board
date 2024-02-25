module DueDateTests exposing (suite)

import Date
import DueDate
import Expect
import Helpers.DecodeHelpers as DecodeTestHelpers
import Test exposing (..)
import TsJson.Encode as TsEncode


suite : Test
suite =
    concat
        [ decoder
        , encoder
        ]


decoder : Test
decoder =
    describe "decoder"
        [ test "decodes a NotSet" <|
            \() ->
                """{"tag":"NotSet"}"""
                    |> DecodeTestHelpers.runDecoder DueDate.decoder
                    |> .decoded
                    |> Expect.equal (Ok DueDate.NotSet)
        , test "decodes a SetToDate" <|
            \() ->
                """{"tag":"SetToDate","date":123456}"""
                    |> DecodeTestHelpers.runDecoder DueDate.decoder
                    |> .decoded
                    |> Expect.equal (Ok <| DueDate.SetToDate (Date.fromRataDie 123456))
        , test "decodes a SetToNone" <|
            \() ->
                """{"tag":"SetToNone"}"""
                    |> DecodeTestHelpers.runDecoder DueDate.decoder
                    |> .decoded
                    |> Expect.equal (Ok DueDate.SetToNone)
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
