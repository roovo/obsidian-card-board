module Form.NamedTagColumn exposing
    ( Error(..)
    , Form
    , decoder
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



-- DECODER


decoder : FD.Decoder Form Error NamedTagColumn
decoder =
    FD.map2 NamedTagColumn.init
        nameDecoder
        tagDecoder



-- PRIVATE


isValidTag : err -> FD.Decoder String err a -> FD.Decoder String err a
isValidTag error d =
    FD.with <|
        \a ->
            if Tag.containsInvalidCharacters a then
                FD.fail error

            else
                d


nameDecoder : FD.Decoder Form Error String
nameDecoder =
    FD.identity
        |> required NameRequired
        |> FD.lift .name


required : err -> FD.Decoder String err a -> FD.Decoder String err a
required error d =
    FD.with <|
        \a ->
            case a of
                "" ->
                    FD.fail error

                _ ->
                    FD.lift identity d


tagDecoder : FD.Decoder Form Error String
tagDecoder =
    FD.identity
        |> required TagRequired
        |> isValidTag InvalidTagCharacters
        |> FD.lift .tag
