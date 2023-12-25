module Form.UntaggedColumn exposing
    ( Error(..)
    , Form
    , decoder
    , init
    , safeDecoder
    , updateName
    )

import Column.Untagged as UntaggedColumn exposing (UntaggedColumn)
import Form.Decoder as FD
import Form.Input as Input
import Form.SafeDecoder as SD



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


safeDecoder : SD.Decoder Form UntaggedColumn
safeDecoder =
    SD.map UntaggedColumn.init safeNameDecoder



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


safeNameDecoder : SD.Decoder Form String
safeNameDecoder =
    SD.identity
        |> SD.lift String.trim
        |> SD.lift .name
