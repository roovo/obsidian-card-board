module Form.NamedTagColumn exposing
    ( Error(..)
    , Form
    , decoder
    , init
    , updateName
    , updateTag
    )

import Column.NamedTag as NamedTagColumn exposing (NamedTagColumn)
import Form.Decoder as FD
import Form.Input as Input
import Tag



-- TYPES


type alias Form =
    { name : String
    , tag : String
    }


type Error
    = NameRequired
    | TagRequired
    | InvalidTagCharacters



-- CONSTRUCTION


init : NamedTagColumn -> Form
init namedTagColumn =
    { name = NamedTagColumn.name namedTagColumn
    , tag = NamedTagColumn.tag namedTagColumn
    }



-- DECODER


decoder : FD.Decoder Form Error NamedTagColumn
decoder =
    FD.map2 NamedTagColumn.init
        nameDecoder
        tagDecoder



-- MODIFICATION


updateName : String -> Form -> Form
updateName newName form =
    { form | name = newName }


updateTag : String -> Form -> Form
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


nameDecoder : FD.Decoder Form Error String
nameDecoder =
    FD.identity
        |> FD.lift String.trim
        |> Input.required NameRequired
        |> FD.lift .name


tagDecoder : FD.Decoder Form Error String
tagDecoder =
    FD.identity
        |> FD.lift String.trim
        |> Input.required TagRequired
        |> isValidTag InvalidTagCharacters
        |> FD.lift .tag
