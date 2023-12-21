module Form.OtherTagsColumn exposing
    ( Error(..)
    , Form
    , decoder
    )

import Column.OtherTags as OtherTagsColumn exposing (OtherTagsColumn)
import Form.Decoder as FD
import Form.Input as Input



-- TYPES


type alias Form =
    { name : String
    }


type Error
    = NameRequired



-- DECODER


decoder : FD.Decoder Form Error OtherTagsColumn
decoder =
    FD.map2 OtherTagsColumn.init
        nameDecoder
        (FD.always [])



-- PRIVATE


nameDecoder : FD.Decoder Form Error String
nameDecoder =
    FD.identity
        |> Input.required NameRequired
        |> FD.lift .name
