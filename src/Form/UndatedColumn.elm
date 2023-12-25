module Form.UndatedColumn exposing
    ( Error(..)
    , Form
    , decoder
    , init
    , safeDecoder
    , updateName
    )

import Column.Undated as UndatedColumn exposing (UndatedColumn)
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


init : UndatedColumn -> Form
init undatedColumn =
    { name = UndatedColumn.name undatedColumn }



-- DECODER


decoder : FD.Decoder Form Error UndatedColumn
decoder =
    FD.map UndatedColumn.init nameDecoder


safeDecoder : SD.Decoder Form UndatedColumn
safeDecoder =
    SD.map UndatedColumn.init safeNameDecoder



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
