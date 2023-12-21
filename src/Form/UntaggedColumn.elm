module Form.UntaggedColumn exposing
    ( Error(..)
    , Form
    , decoder
    , init
    )

import Column.Untagged as UntaggedColumn exposing (UntaggedColumn)
import Form.Decoder as FD
import Form.Input as Input



-- TYPES


type alias Form =
    { name : String
    }


type Error
    = NameRequired



-- CONSTRUCTION


init : UntaggedColumn -> Form
init untaggedColumn =
    { name = UntaggedColumn.name untaggedColumn }



-- DECODER


decoder : FD.Decoder Form Error UntaggedColumn
decoder =
    FD.map UntaggedColumn.init nameDecoder



-- PRIVATE


nameDecoder : FD.Decoder Form Error String
nameDecoder =
    FD.identity
        |> Input.required NameRequired
        |> FD.lift .name
