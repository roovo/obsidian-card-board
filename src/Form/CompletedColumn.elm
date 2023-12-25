module Form.CompletedColumn exposing
    ( Error(..)
    , Form
    , LimitError(..)
    , decoder
    , init
    , safeDecoder
    , updateLimit
    , updateName
    )

import Column.Completed as CompletedColumn exposing (CompletedColumn)
import Form.Decoder as FD
import Form.Input as Input
import Form.SafeDecoder as SD



-- TYPES


type alias Form =
    { name : String
    , limit : String
    }


type Error
    = NameRequired
    | LimitError LimitError
    | LimitRequired


type LimitError
    = InvalidInt
    | NotPositive



-- CONSTRUCTION


init : CompletedColumn -> Form
init completedColumn =
    { name = CompletedColumn.name completedColumn
    , limit = String.fromInt <| CompletedColumn.limit completedColumn
    }



-- DECODER


decoder : FD.Decoder Form Error CompletedColumn
decoder =
    FD.map2 fromForm
        nameDecoder
        limitDecoder


safeDecoder : SD.Decoder Form CompletedColumn
safeDecoder =
    SD.map2 fromForm
        safeNameDecoder
        safeLimitDecoder



-- MODIFICATION


updateLimit : String -> Form -> Form
updateLimit newLimit form =
    { form | limit = newLimit }


updateName : String -> Form -> Form
updateName newName form =
    { form | name = newName }



-- PRIVATE


fromForm : String -> Int -> CompletedColumn
fromForm name_ limit_ =
    CompletedColumn.init name_ 0 limit_


limitDecoder : FD.Decoder Form Error Int
limitDecoder =
    FD.int InvalidInt
        |> FD.lift String.trim
        |> FD.assert (FD.minBound NotPositive 1)
        |> FD.mapError LimitError
        |> Input.required LimitRequired
        |> FD.lift .limit


nameDecoder : FD.Decoder Form Error String
nameDecoder =
    FD.identity
        |> FD.lift String.trim
        |> Input.required NameRequired
        |> FD.lift .name


safeLimitDecoder : SD.Decoder Form Int
safeLimitDecoder =
    SD.int 10
        |> SD.lift String.trim
        |> SD.assert (SD.minBound 10 1)
        |> SD.lift .limit


safeNameDecoder : SD.Decoder Form String
safeNameDecoder =
    SD.identity
        |> SD.lift String.trim
        |> SD.lift .name
