module Form.Column.Completed exposing
    ( CompletedColumnForm
    , Error(..)
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


type alias CompletedColumnForm =
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


init : CompletedColumn -> CompletedColumnForm
init completedColumn =
    { name = CompletedColumn.name completedColumn
    , limit = String.fromInt <| CompletedColumn.limit completedColumn
    }



-- DECODER


decoder : FD.Decoder CompletedColumnForm Error CompletedColumn
decoder =
    FD.map2 fromForm
        nameDecoder
        limitDecoder


safeDecoder : SD.Decoder CompletedColumnForm CompletedColumn
safeDecoder =
    SD.map2 fromForm
        safeNameDecoder
        safeLimitDecoder



-- MODIFICATION


updateLimit : String -> CompletedColumnForm -> CompletedColumnForm
updateLimit newLimit form =
    { form | limit = newLimit }


updateName : String -> CompletedColumnForm -> CompletedColumnForm
updateName newName form =
    { form | name = newName }



-- PRIVATE


fromForm : String -> Int -> CompletedColumn
fromForm name_ limit_ =
    CompletedColumn.init name_ 0 limit_


limitDecoder : FD.Decoder CompletedColumnForm Error Int
limitDecoder =
    FD.int InvalidInt
        |> FD.lift String.trim
        |> FD.assert (FD.minBound NotPositive 1)
        |> FD.mapError LimitError
        |> Input.required LimitRequired
        |> FD.lift .limit


nameDecoder : FD.Decoder CompletedColumnForm Error String
nameDecoder =
    FD.identity
        |> FD.lift String.trim
        |> Input.required NameRequired
        |> FD.lift .name


safeLimitDecoder : SD.Decoder CompletedColumnForm Int
safeLimitDecoder =
    SD.int 10
        |> SD.lift String.trim
        |> SD.assert (SD.minBound 10 1)
        |> SD.lift .limit


safeNameDecoder : SD.Decoder CompletedColumnForm String
safeNameDecoder =
    SD.identity
        |> SD.lift String.trim
        |> SD.lift .name
