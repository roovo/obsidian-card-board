module Form.CompletedColumn exposing
    ( Error(..)
    , Form
    , LimitError(..)
    , decoder
    , init
    , updateLimit
    , updateName
    )

import Column.Completed as CompletedColumn exposing (CompletedColumn)
import Form.Decoder as FD
import Form.Input as Input



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



-- MODIFICATION


updateLimit : String -> Form -> Form
updateLimit newLimit form =
    { form | limit = newLimit }


updateName : String -> Form -> Form
updateName newName form =
    { form | name = newName }



-- PRIVATE


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


fromForm : String -> Int -> CompletedColumn
fromForm name_ limit_ =
    CompletedColumn.init name_ 0 limit_
