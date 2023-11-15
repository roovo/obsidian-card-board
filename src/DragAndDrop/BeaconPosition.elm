module DragAndDrop.BeaconPosition exposing
    ( BeaconPosition(..)
    , decoder
    , encoder
    , identifier
    )

import Json.Decode as JD
import Json.Encode as JE



-- TYPES


type BeaconPosition
    = Before String
    | After String



-- ENCODE / DECODE


decoder : JD.Decoder BeaconPosition
decoder =
    JD.map2
        Tuple.pair
        (JD.field "position" JD.string)
        (JD.field "identifier" JD.string)
        |> JD.andThen toPosition


encoder : BeaconPosition -> JE.Value
encoder beaconPosition =
    let
        ( positionStr, identifierString ) =
            case beaconPosition of
                After tabIndex ->
                    ( "after", tabIndex )

                Before tabIndex ->
                    ( "before", tabIndex )
    in
    JE.object
        [ ( "position", JE.string positionStr )
        , ( "identifier", JE.string identifierString )
        ]



-- UTILS


identifier : BeaconPosition -> String
identifier beaconPosition =
    case beaconPosition of
        Before id ->
            id

        After id ->
            id



-- PRIVATE


toPosition : ( String, String ) -> JD.Decoder BeaconPosition
toPosition ( position, identifier_ ) =
    case position of
        "before" ->
            JD.succeed (Before identifier_)

        "after" ->
            JD.succeed (After identifier_)

        _ ->
            JD.fail ("Unknown position: " ++ position)
