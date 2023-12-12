module DragAndDrop.BeaconPosition exposing
    ( BeaconPosition(..)
    , decoder
    , encoder
    , performMove
    , uniqueId
    )

import Json.Encode as JE
import List.Extra as LE
import TsJson.Decode as TsDecode



-- TYPES


type BeaconPosition
    = Before String
    | After String



-- ENCODE / DECODE


decoder : TsDecode.Decoder BeaconPosition
decoder =
    TsDecode.oneOf
        [ toElmBeacon "after" After TsDecode.string
        , toElmBeacon "before" Before TsDecode.string
        ]


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
        , ( "uniqueId", JE.string identifierString )
        ]



-- UTILS


performMove : String -> BeaconPosition -> (a -> String) -> List a -> List a
performMove itemId beaconPosition idFunction items =
    let
        afterRemoving : List a
        afterRemoving =
            List.filter (\i -> idFunction i /= itemId) items

        insertItem : a -> Result String (List a)
        insertItem item =
            case LE.findIndex (\i -> idFunction i == uniqueId beaconPosition) afterRemoving of
                Nothing ->
                    Err ""

                Just beaconIndex ->
                    case beaconPosition of
                        After _ ->
                            Ok
                                (List.concat
                                    [ List.take (beaconIndex + 1) afterRemoving
                                    , [ item ]
                                    , List.drop (beaconIndex + 1) afterRemoving
                                    ]
                                )

                        Before _ ->
                            Ok
                                (List.concat
                                    [ List.take beaconIndex afterRemoving
                                    , [ item ]
                                    , List.drop beaconIndex afterRemoving
                                    ]
                                )
    in
    items
        |> LE.find (\i -> idFunction i == itemId)
        |> Maybe.map insertItem
        |> Maybe.map (Result.withDefault items)
        |> Maybe.withDefault items


uniqueId : BeaconPosition -> String
uniqueId beaconPosition =
    case beaconPosition of
        Before id ->
            id

        After id ->
            id



-- PRIVATE


toElmBeacon : String -> (value -> a) -> TsDecode.Decoder value -> TsDecode.Decoder a
toElmBeacon tagName constructor decoder_ =
    TsDecode.field "position" (TsDecode.literal constructor (JE.string tagName))
        |> TsDecode.andMap (TsDecode.field "uniqueId" decoder_)
