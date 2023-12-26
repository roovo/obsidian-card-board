module Form.Column.OtherTags exposing
    ( Error(..)
    , OtherTagsColumnForm
    , decoder
    , init
    , safeDecoder
    , updateName
    )

import Column.OtherTags as OtherTagsColumn exposing (OtherTagsColumn)
import Form.Decoder as FD
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


decoder : FD.Decoder OtherTagsColumnForm Error OtherTagsColumn
decoder =
    FD.map2 OtherTagsColumn.init
        nameDecoder
        (FD.always [])


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


nameDecoder : FD.Decoder OtherTagsColumnForm Error String
nameDecoder =
    FD.identity
        |> FD.lift String.trim
        |> Input.required NameRequired
        |> FD.lift .name


safeNameDecoder : SD.Decoder OtherTagsColumnForm String
safeNameDecoder =
    SD.identity
        |> SD.lift String.trim
        |> SD.lift .name
