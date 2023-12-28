module Form.Column.Untagged exposing
    ( UntaggedColumnForm
    , init
    , safeDecoder
    , updateName
    )

import Column.Untagged as UntaggedColumn exposing (UntaggedColumn)
import Form.SafeDecoder as SD



-- TYPES


type alias UntaggedColumnForm =
    { name : String
    }



-- CONSTRUCTION


init : UntaggedColumn -> UntaggedColumnForm
init untaggedColumn =
    { name = UntaggedColumn.name untaggedColumn }



-- DECODER


safeDecoder : SD.Decoder UntaggedColumnForm UntaggedColumn
safeDecoder =
    SD.map UntaggedColumn.init safeNameDecoder



-- MODIFICATION


updateName : String -> UntaggedColumnForm -> UntaggedColumnForm
updateName newName form =
    { form | name = newName }



-- PRIVATE


safeNameDecoder : SD.Decoder UntaggedColumnForm String
safeNameDecoder =
    SD.identity
        |> SD.lift String.trim
        |> SD.lift .name
