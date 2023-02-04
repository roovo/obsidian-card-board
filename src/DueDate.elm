module DueDate exposing (DueDate(..))

import Date exposing (Date)


type DueDate
    = SetToDate Date
    | SetToNone
    | NotSet
