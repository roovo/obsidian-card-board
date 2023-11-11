module DragAndDrop.BeaconPosition exposing
    ( BeaconPosition(..)
    , encoder
    , identifier
    )

import Json.Encode as JE



-- TYPES


type BeaconPosition
    = Before String
    | After String



-- ENCODING


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
