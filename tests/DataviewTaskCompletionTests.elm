module DataviewTaskCompletionTests exposing (suite)

import DataviewTaskCompletion
import Expect
import Fuzz exposing (Fuzzer)
import Helpers.DecodeHelpers as DecodeHelpers
import Test exposing (..)


suite : Test
suite =
    concat
        [ decoder
        , default
        ]


decoder : Test
decoder =
    describe "decoder"
        [ fuzz2 boolString textString "decodes all taskCompletionText:false combinations to NoCompletion" <|
            \emoji text ->
                "{\"taskCompletionTracking\":false,\"taskCompletionUseEmojiShorthand\":"
                    ++ emoji
                    ++ ",\"taskCompletionText\":\""
                    ++ text
                    ++ "\"}"
                    |> DecodeHelpers.runDecoder DataviewTaskCompletion.decoder
                    |> .decoded
                    |> Expect.equal (Ok DataviewTaskCompletion.NoCompletion)
        , fuzz textString "decodes all taskCompletionText:false taskCompletionUseEmojiShorthand:true combinations to Emoji" <|
            \text ->
                "{\"taskCompletionTracking\":true,\"taskCompletionUseEmojiShorthand\":true,\"taskCompletionText\":\""
                    ++ text
                    ++ "\"}"
                    |> DecodeHelpers.runDecoder DataviewTaskCompletion.decoder
                    |> .decoded
                    |> Expect.equal (Ok DataviewTaskCompletion.Emoji)
        , fuzz textString "decodes all taskCompletionText:false taskCompletionUseEmojiShorthand:false combinations to Text" <|
            \text ->
                "{\"taskCompletionTracking\":true,\"taskCompletionUseEmojiShorthand\":false,\"taskCompletionText\":\""
                    ++ text
                    ++ "\"}"
                    |> DecodeHelpers.runDecoder DataviewTaskCompletion.decoder
                    |> .decoded
                    |> Expect.equal (Ok <| DataviewTaskCompletion.Text text)
        ]


default : Test
default =
    describe "default"
        [ test "is Text 'completion'" <|
            \() ->
                DataviewTaskCompletion.default
                    |> Expect.equal (DataviewTaskCompletion.Text "completion")
        ]



-- HELPERS


boolString : Fuzzer String
boolString =
    Fuzz.bool
        |> Fuzz.map
            (\b ->
                if b then
                    "true"

                else
                    "false"
            )


textString : Fuzzer String
textString =
    let
        dropInvalidCharacters : String -> String
        dropInvalidCharacters a =
            String.toList a
                |> List.filter Char.isAlphaNum
                |> String.fromList
    in
    Fuzz.map dropInvalidCharacters Fuzz.string
