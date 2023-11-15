module DragAndDrop.BeaconPositionTests exposing (suite)

import DragAndDrop.BeaconPosition as BeaconPosition exposing (BeaconPosition(..))
import Expect
import Json.Decode as JD
import Json.Encode as JE
import Test exposing (..)


suite : Test
suite =
    concat
        [ decoder
        , encoder
        , identifier
        ]


decoder : Test
decoder =
    describe "decoder"
        [ test "decodes an After" <|
            \() ->
                """{"position":"after","identifier":"some identifier"}"""
                    |> JD.decodeString BeaconPosition.decoder
                    |> Expect.equal (Ok <| After "some identifier")
        , test "decodes a Before" <|
            \() ->
                """{"position":"before","identifier":"some identifier"}"""
                    |> JD.decodeString BeaconPosition.decoder
                    |> Expect.equal (Ok <| Before "some identifier")
        ]


encoder : Test
encoder =
    describe "encoder"
        [ test "encodes a Before" <|
            \() ->
                Before "some identifier"
                    |> BeaconPosition.encoder
                    |> JE.encode 0
                    |> Expect.equal """{"position":"before","identifier":"some identifier"}"""
        , test "encodes an After" <|
            \() ->
                After "some identifier"
                    |> BeaconPosition.encoder
                    |> JE.encode 0
                    |> Expect.equal """{"position":"after","identifier":"some identifier"}"""
        ]


identifier : Test
identifier =
    describe "identifier"
        [ test "extracts the identifier from a Before" <|
            \() ->
                Before "some identifier"
                    |> BeaconPosition.identifier
                    |> Expect.equal "some identifier"
        , test "extracts the identifier from an After" <|
            \() ->
                After "some identifier"
                    |> BeaconPosition.identifier
                    |> Expect.equal "some identifier"
        ]
