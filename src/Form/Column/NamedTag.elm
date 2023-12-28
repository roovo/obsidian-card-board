module Form.Column.NamedTag exposing
    ( Error(..)
    , NamedTagColumnForm
    , init
    , safeDecoder
    , updateName
    , updateTag
    )

import Column.NamedTag as NamedTagColumn exposing (NamedTagColumn)
import Form.Input as Input
import Form.SafeDecoder as SD
import Tag



-- TYPES


type alias NamedTagColumnForm =
    { name : String
    , tag : String
    }


type Error
    = NameRequired
    | TagRequired
    | InvalidTagCharacters



-- CONSTRUCTION


init : NamedTagColumn -> NamedTagColumnForm
init namedTagColumn =
    { name = NamedTagColumn.name namedTagColumn
    , tag = NamedTagColumn.tag namedTagColumn
    }



-- DECODER


safeDecoder : SD.Decoder NamedTagColumnForm NamedTagColumn
safeDecoder =
    SD.map2 NamedTagColumn.init
        safeNameDecoder
        safeTagDecoder



-- MODIFICATION


updateName : String -> NamedTagColumnForm -> NamedTagColumnForm
updateName newName form =
    { form | name = newName }


updateTag : String -> NamedTagColumnForm -> NamedTagColumnForm
updateTag newName form =
    { form | tag = newName }



-- PRIVATE


safeNameDecoder : SD.Decoder NamedTagColumnForm String
safeNameDecoder =
    SD.identity
        |> SD.lift String.trim
        |> SD.lift .name


safeTagDecoder : SD.Decoder NamedTagColumnForm String
safeTagDecoder =
    SD.identity
        |> SD.lift String.trim
        |> SD.lift .tag
