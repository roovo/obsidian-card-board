module Form.Column.Undated exposing
    ( Error(..)
    , UndatedColumnForm
    , init
    , safeDecoder
    , updateName
    )

import Column.Undated as UndatedColumn exposing (UndatedColumn)
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


safeDecoder : SD.Decoder UndatedColumnForm UndatedColumn
safeDecoder =
    SD.map UndatedColumn.init safeNameDecoder



-- MODIFICATION


updateName : String -> UndatedColumnForm -> UndatedColumnForm
updateName newName form =
    { form | name = newName }



-- PRIVATE


safeNameDecoder : SD.Decoder UndatedColumnForm String
safeNameDecoder =
    SD.identity
        |> SD.lift String.trim
        |> SD.lift .name
