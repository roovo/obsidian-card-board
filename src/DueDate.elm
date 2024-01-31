module DueDate exposing
    ( DueDate(..)
    , encoder
    )

import Date exposing (Date)
import EncodeHelpers
import TsJson.Encode as TsEncode


type DueDate
    = NotSet
    | SetToDate Date
    | SetToNone



-- SERIALIZE


encoder : TsEncode.Encoder DueDate
encoder =
    TsEncode.union
        (\vNotSet vSetToDate vSetToNone value ->
            case value of
                NotSet ->
                    vNotSet

                SetToDate date ->
                    vSetToDate date

                SetToNone ->
                    vSetToNone
        )
        |> TsEncode.variant0 "NotSet"
        |> TsEncode.variantObject "SetToDate" [ TsEncode.required "date" identity EncodeHelpers.dateEncoder ]
        |> TsEncode.variant0 "SetToNone"
        |> TsEncode.buildUnion
