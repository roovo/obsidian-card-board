module DefaultColumnNamesTests exposing (suite)

import DefaultColumnNames exposing (DefaultColumnNames)
import Expect
import Helpers.DecodeHelpers as DecodeHelpers
import Test exposing (..)
import TsJson.Encode as TsEncode


suite : Test
suite =
    concat
        [ encodeDecode
        , nameFor
        ]


encodeDecode : Test
encodeDecode =
    describe "encodeDecode"
        [ test "can decode the encoded default back to the original" <|
            \() ->
                DefaultColumnNames.default
                    |> TsEncode.runExample DefaultColumnNames.encoder
                    |> .output
                    |> DecodeHelpers.runDecoder DefaultColumnNames.v_0_12_0_decoder
                    |> .decoded
                    |> Expect.equal (Ok DefaultColumnNames.default)
        , test "can decode an encoded non default back to the original" <|
            \() ->
                exampleColumnNames
                    |> DecodeHelpers.runDecoder DefaultColumnNames.v_0_12_0_decoder
                    |> .decoded
                    |> Result.withDefault DefaultColumnNames.default
                    |> TsEncode.runExample DefaultColumnNames.encoder
                    |> .output
                    |> Expect.equal exampleColumnNames
        ]


nameFor : Test
nameFor =
    describe "nameFor"
        [ test "default for today is Today" <|
            \() ->
                DefaultColumnNames.default
                    |> DefaultColumnNames.nameFor "today"
                    |> Expect.equal "Today"
        , test "default for tomorrow is Tomorrow" <|
            \() ->
                DefaultColumnNames.default
                    |> DefaultColumnNames.nameFor "tomorrow"
                    |> Expect.equal "Tomorrow"
        , test "default for future is Future" <|
            \() ->
                DefaultColumnNames.default
                    |> DefaultColumnNames.nameFor "future"
                    |> Expect.equal "Future"
        , test "default for undated is Undated" <|
            \() ->
                DefaultColumnNames.default
                    |> DefaultColumnNames.nameFor "undated"
                    |> Expect.equal "Undated"
        , test "default for otherTags is Other Tags" <|
            \() ->
                DefaultColumnNames.default
                    |> DefaultColumnNames.nameFor "otherTags"
                    |> Expect.equal "Other Tags"
        , test "default for untagged is Untagged" <|
            \() ->
                DefaultColumnNames.default
                    |> DefaultColumnNames.nameFor "untagged"
                    |> Expect.equal "Untagged"
        , test "default for completed is Completed" <|
            \() ->
                DefaultColumnNames.default
                    |> DefaultColumnNames.nameFor "completed"
                    |> Expect.equal "Completed"
        , test "default for an un-recognised column is ''" <|
            \() ->
                DefaultColumnNames.default
                    |> DefaultColumnNames.nameFor "xxxxx"
                    |> Expect.equal ""
        , test "can set the value for today" <|
            \() ->
                { defaultNames | today = Just "Foo" }
                    |> DefaultColumnNames.nameFor "today"
                    |> Expect.equal "Foo"
        , test "can set the value for tomorrow" <|
            \() ->
                { defaultNames | tomorrow = Just "Foo" }
                    |> DefaultColumnNames.nameFor "tomorrow"
                    |> Expect.equal "Foo"
        , test "can set the value for future" <|
            \() ->
                { defaultNames | future = Just "Foo" }
                    |> DefaultColumnNames.nameFor "future"
                    |> Expect.equal "Foo"
        , test "can set the value for undated" <|
            \() ->
                { defaultNames | undated = Just "Foo" }
                    |> DefaultColumnNames.nameFor "undated"
                    |> Expect.equal "Foo"
        , test "can set the value for otherTags" <|
            \() ->
                { defaultNames | otherTags = Just "Foo" }
                    |> DefaultColumnNames.nameFor "otherTags"
                    |> Expect.equal "Foo"
        , test "can set the value for untagged" <|
            \() ->
                { defaultNames | untagged = Just "Foo" }
                    |> DefaultColumnNames.nameFor "untagged"
                    |> Expect.equal "Foo"
        , test "can set the value for completed" <|
            \() ->
                { defaultNames | completed = Just "Foo" }
                    |> DefaultColumnNames.nameFor "completed"
                    |> Expect.equal "Foo"
        ]



-- HELPERS


defaultNames : DefaultColumnNames
defaultNames =
    DefaultColumnNames.default


exampleColumnNames : String
exampleColumnNames =
    """{"today":"col1","tomorrow":"col2","future":"col3","undated":"col4","otherTags":"col4","untagged":"col6","completed":"col7"}"""
