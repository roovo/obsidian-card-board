module Form.OtherTagsColumn exposing
    ( Error(..)
    , Form
    , decoder
    , init
    , updateName
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



-- CONSTRUCTION


init : OtherTagsColumn -> Form
init otherTagsColumn =
    { name = OtherTagsColumn.name otherTagsColumn }



-- DECODER


decoder : FD.Decoder Form Error OtherTagsColumn
decoder =
    FD.map2 OtherTagsColumn.init
        nameDecoder
        (FD.always [])



-- MODIFICATION


updateName : String -> Form -> Form
updateName newName form =
    { form | name = newName }



-- PRIVATE


nameDecoder : FD.Decoder Form Error String
nameDecoder =
    FD.identity
        |> FD.lift String.trim
        |> Input.required NameRequired
        |> FD.lift .name
