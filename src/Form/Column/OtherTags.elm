module Form.Column.OtherTags exposing
    ( Error(..)
    , OtherTagsColumnForm
    , init
    , safeDecoder
    , updateName
    )

import Column.OtherTags as OtherTagsColumn exposing (OtherTagsColumn)
import Form.Input as Input
import Form.SafeDecoder as SD



-- TYPES


type alias OtherTagsColumnForm =
    { name : String
    }


type Error
    = NameRequired



-- CONSTRUCTION


init : OtherTagsColumn -> OtherTagsColumnForm
init otherTagsColumn =
    { name = OtherTagsColumn.name otherTagsColumn }



-- DECODER


safeDecoder : SD.Decoder OtherTagsColumnForm OtherTagsColumn
safeDecoder =
    SD.map2 OtherTagsColumn.init
        safeNameDecoder
        (SD.always [])



-- MODIFICATION


updateName : String -> OtherTagsColumnForm -> OtherTagsColumnForm
updateName newName form =
    { form | name = newName }



-- PRIVATE


safeNameDecoder : SD.Decoder OtherTagsColumnForm String
safeNameDecoder =
    SD.identity
        |> SD.lift String.trim
        |> SD.lift .name
