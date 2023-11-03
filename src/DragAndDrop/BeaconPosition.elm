module DragAndDrop.BeaconPosition exposing
    ( BeaconPosition(..)
    , encode
    )

import Json.Encode as JE



-- TYPES


type BeaconPosition
    = Before String
    | After String



-- ENCODING


encode : BeaconPosition -> JE.Value
encode beaconPosition =
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
