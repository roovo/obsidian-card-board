module Form.UndatedColumn exposing
    ( Error(..)
    , Form
    , decoder
    )

import Column.Undated as UndatedColumn exposing (UndatedColumn)
import Form.Decoder as FD
import Form.Input as Input



-- TYPES


type alias Form =
    { name : String
    }


type Error
    = NameRequired



-- DECODER


decoder : FD.Decoder Form Error UndatedColumn
decoder =
    FD.map UndatedColumn.init nameDecoder



-- PRIVATE


nameDecoder : FD.Decoder Form Error String
nameDecoder =
    FD.identity
        |> Input.required NameRequired
        |> FD.lift .name
