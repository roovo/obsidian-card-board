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
        , performMove
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


performMove : Test
performMove =
    describe "performMove"
        [ test "does nothing for an empty list" <|
            \() ->
                []
                    |> BeaconPosition.performMove "0" (BeaconPosition.After "0") (always "")
                    |> Expect.equal []
        , test "does nothing if moving a single item to the same index (0)" <|
            \() ->
                [ "1" ]
                    |> BeaconPosition.performMove "0" (BeaconPosition.After "0") identity
                    |> Expect.equal [ "1" ]
        , test "moves an item to the previous position using Before" <|
            \() ->
                [ "0", "1", "2", "3", "4" ]
                    |> BeaconPosition.performMove "2" (BeaconPosition.Before "1") identity
                    |> Expect.equal [ "0", "2", "1", "3", "4" ]
        , test "moves an item to the previous position using After" <|
            \() ->
                [ "0", "1", "2", "3", "4" ]
                    |> BeaconPosition.performMove "2" (BeaconPosition.After "0") identity
                    |> Expect.equal [ "0", "2", "1", "3", "4" ]
        , test "moves an item to the next position using Before" <|
            \() ->
                [ "0", "1", "2", "3", "4" ]
                    |> BeaconPosition.performMove "2" (BeaconPosition.Before "4") identity
                    |> Expect.equal [ "0", "1", "3", "2", "4" ]
        , test "moves an item to the next position using After" <|
            \() ->
                [ "0", "1", "2", "3", "4" ]
                    |> BeaconPosition.performMove "2" (BeaconPosition.After "3") identity
                    |> Expect.equal [ "0", "1", "3", "2", "4" ]
        , test "moves an item to the first position using Before" <|
            \() ->
                [ "0", "1", "2", "3", "4" ]
                    |> BeaconPosition.performMove "2" (BeaconPosition.Before "0") identity
                    |> Expect.equal [ "2", "0", "1", "3", "4" ]
        , test "moves an item to the last position using After" <|
            \() ->
                [ "0", "1", "2", "3", "4" ]
                    |> BeaconPosition.performMove "2" (BeaconPosition.After "4") identity
                    |> Expect.equal [ "0", "1", "3", "4", "2" ]
        , test "returns the same list if the position to be moved to does not exist" <|
            \() ->
                [ "0", "1", "2", "3", "4" ]
                    |> BeaconPosition.performMove "2" (BeaconPosition.After "55") identity
                    |> Expect.equal [ "0", "1", "2", "3", "4" ]
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
