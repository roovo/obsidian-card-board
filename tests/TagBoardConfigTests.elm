module TagBoardConfigTests exposing (suite)

import Expect
import Helpers.BoardConfigHelpers as BoardConfigHelpers
import Helpers.DecodeHelpers as DecodeHelpers
import Parser
import TagBoardConfig exposing (TagBoardConfig)
import Test exposing (..)
import TsJson.Encode as TsEncode


suite : Test
suite =
    concat
        [ columnConfigsParserTest
        , encodeDecode
        ]


columnConfigsParserTest : Test
columnConfigsParserTest =
    describe "parsing tags with headers"
        [ test "parses an empty string to an empty config" <|
            \() ->
                ""
                    |> Parser.run TagBoardConfig.columnConfigsParser
                    |> Expect.equal (Ok [])
        , test "parses an string containing a single word" <|
            \() ->
                "foo"
                    |> Parser.run TagBoardConfig.columnConfigsParser
                    |> Expect.equal (Ok [ TagBoardConfig.LocalColumnConfig "foo" "Foo" ])
        , test "parses '#foo'" <|
            \() ->
                "#foo"
                    |> Parser.run TagBoardConfig.columnConfigsParser
                    |> Expect.equal (Ok [ TagBoardConfig.LocalColumnConfig "foo" "Foo" ])
        , test "parses '#foo/bar'" <|
            \() ->
                "#foo/bar"
                    |> Parser.run TagBoardConfig.columnConfigsParser
                    |> Expect.equal (Ok [ TagBoardConfig.LocalColumnConfig "foo/bar" "Foo bar" ])
        , test "parses '#foo/bar baz'" <|
            \() ->
                "#foo/bar baz"
                    |> Parser.run TagBoardConfig.columnConfigsParser
                    |> Expect.equal (Ok [ TagBoardConfig.LocalColumnConfig "foo/bar" "baz" ])
        , test "parses '#foo/bar baz bax'" <|
            \() ->
                "#foo/bar baz bax"
                    |> Parser.run TagBoardConfig.columnConfigsParser
                    |> Expect.equal (Ok [ TagBoardConfig.LocalColumnConfig "foo/bar" "baz bax" ])
        , test "parses '   #foo/bar     baz     bax    '" <|
            \() ->
                "   #foo/bar     baz     bax    "
                    |> Parser.run TagBoardConfig.columnConfigsParser
                    |> Expect.equal (Ok [ TagBoardConfig.LocalColumnConfig "foo/bar" "baz bax" ])
        , test "parses multilines" <|
            \() ->
                """#foo     bar     baz
#aa"""
                    |> Parser.run TagBoardConfig.columnConfigsParser
                    |> Expect.equal
                        (Ok
                            [ TagBoardConfig.LocalColumnConfig "foo" "bar baz"
                            , TagBoardConfig.LocalColumnConfig "aa" "Aa"
                            ]
                        )
        ]


encodeDecode : Test
encodeDecode =
    describe "encoding and decoding config"
        [ test "can decode the encoded string back to the original" <|
            \() ->
                exampleConfig
                    |> TsEncode.runExample TagBoardConfig.encoder
                    |> .output
                    |> DecodeHelpers.runDecoder TagBoardConfig.decoder_v_0_5_0
                    |> .decoded
                    |> Expect.equal (Ok exampleConfig)
        ]



-- HELPERS


exampleConfig : TagBoardConfig
exampleConfig =
    BoardConfigHelpers.exampleTagBoardConfig
