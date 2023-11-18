module DragAndDrop.BeaconPositionTests exposing (suite)

import DragAndDrop.BeaconPosition as BeaconPosition exposing (BeaconPosition(..))
import Expect
import Helpers.DecodeHelpers as DecodeHelpers
import Json.Encode as JE
import Test exposing (..)


suite : Test
suite =
    concat
        [ decoder
        , encoder
        , uniqueId
        ]


decoder : Test
decoder =
    describe "decoder"
        [ test "decodes an After" <|
            \() ->
                """{"position":"after","uniqueId":"some uniqueId"}"""
                    |> DecodeHelpers.runDecoder BeaconPosition.decoder
                    |> .decoded
                    |> Expect.equal (Ok <| After "some uniqueId")
        , test "decodes a Before" <|
            \() ->
                """{"position":"before","uniqueId":"some uniqueId"}"""
                    |> DecodeHelpers.runDecoder BeaconPosition.decoder
                    |> .decoded
                    |> Expect.equal (Ok <| Before "some uniqueId")
        ]


encoder : Test
encoder =
    describe "encoder"
        [ test "encodes a Before" <|
            \() ->
                Before "some uniqueId"
                    |> BeaconPosition.encoder
                    |> JE.encode 0
                    |> Expect.equal """{"position":"before","uniqueId":"some uniqueId"}"""
        , test "encodes an After" <|
            \() ->
                After "some uniqueId"
                    |> BeaconPosition.encoder
                    |> JE.encode 0
                    |> Expect.equal """{"position":"after","uniqueId":"some uniqueId"}"""
        ]


uniqueId : Test
uniqueId =
    describe "uniqueId"
        [ test "extracts the uniqueId from a Before" <|
            \() ->
                Before "some uniqueId"
                    |> BeaconPosition.uniqueId
                    |> Expect.equal "some uniqueId"
        , test "extracts the uniqueId from an After" <|
            \() ->
                After "some uniqueId"
                    |> BeaconPosition.uniqueId
                    |> Expect.equal "some uniqueId"
        ]
