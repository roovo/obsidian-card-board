module Form.Column.NamedTag exposing
    ( Error(..)
    , NamedTagColumnForm
    , decoder
    , init
    , safeDecoder
    , updateName
    , updateTag
    )

import Column.NamedTag as NamedTagColumn exposing (NamedTagColumn)
import Form.Decoder as FD
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


decoder : FD.Decoder NamedTagColumnForm Error NamedTagColumn
decoder =
    FD.map2 NamedTagColumn.init
        nameDecoder
        tagDecoder


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


isValidTag : err -> FD.Decoder String err a -> FD.Decoder String err a
isValidTag error d =
    FD.with <|
        \a ->
            if Tag.containsInvalidCharacters (String.trim a) then
                FD.fail error

            else
                d


nameDecoder : FD.Decoder NamedTagColumnForm Error String
nameDecoder =
    FD.identity
        |> FD.lift String.trim
        |> Input.required NameRequired
        |> FD.lift .name


tagDecoder : FD.Decoder NamedTagColumnForm Error String
tagDecoder =
    FD.identity
        |> FD.lift String.trim
        |> Input.required TagRequired
        |> isValidTag InvalidTagCharacters
        |> FD.lift .tag


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
