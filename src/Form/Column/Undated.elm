module Form.Column.Undated exposing
    ( Error(..)
    , UndatedColumnForm
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


type alias UndatedColumnForm =
    { name : String
    }


type Error
    = NameRequired



-- CONSTRUCTION


init : UndatedColumn -> UndatedColumnForm
init undatedColumn =
    { name = UndatedColumn.name undatedColumn }



-- DECODER


decoder : FD.Decoder UndatedColumnForm Error UndatedColumn
decoder =
    FD.map UndatedColumn.init nameDecoder


safeDecoder : SD.Decoder UndatedColumnForm UndatedColumn
safeDecoder =
    SD.map UndatedColumn.init safeNameDecoder



-- MODIFICATION


updateName : String -> UndatedColumnForm -> UndatedColumnForm
updateName newName form =
    { form | name = newName }



-- PRIVATE


nameDecoder : FD.Decoder UndatedColumnForm Error String
nameDecoder =
    FD.identity
        |> FD.lift String.trim
        |> Input.required NameRequired
        |> FD.lift .name


safeNameDecoder : SD.Decoder UndatedColumnForm String
safeNameDecoder =
    SD.identity
        |> SD.lift String.trim
        |> SD.lift .name
