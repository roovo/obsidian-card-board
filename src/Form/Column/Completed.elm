module Form.Column.Completed exposing
    ( CompletedColumnForm
    , init
    , safeDecoder
    , updateLimit
    , updateName
    )

import Column.Completed as CompletedColumn exposing (CompletedColumn)
import Form.SafeDecoder as SD



-- TYPES


type alias CompletedColumnForm =
    { name : String
    , limit : String
    }



-- CONSTRUCTION


init : CompletedColumn -> CompletedColumnForm
init completedColumn =
    { name = CompletedColumn.name completedColumn
    , limit = String.fromInt <| CompletedColumn.limit completedColumn
    }



-- DECODER


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
