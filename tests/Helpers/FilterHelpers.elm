module Helpers.FilterHelpers exposing
    ( exampleFilters
    , fileFilter
    , pathFilter
    , tagFilter
    )

import Filter exposing (Filter, Polarity)
import Helpers.DecodeHelpers as DecodeHelpers
import TsJson.Decode as TsDecode


fileFilter : String -> Filter
fileFilter file =
    "{\"tag\":\"fileFilter\",\"data\":\""
        ++ file
        ++ "\"}"
        |> DecodeHelpers.runDecoder Filter.decoder
        |> .decoded
        |> Result.withDefault Filter.dummy


pathFilter : String -> Filter
pathFilter path =
    "{\"tag\":\"pathFilter\",\"data\":\""
        ++ path
        ++ "\"}"
        |> DecodeHelpers.runDecoder Filter.decoder
        |> .decoded
        |> Result.withDefault Filter.dummy


tagFilter : String -> Filter
tagFilter tag =
    "{\"tag\":\"tagFilter\",\"data\":\""
        ++ tag
        ++ "\"}"
        |> DecodeHelpers.runDecoder Filter.decoder
        |> .decoded
        |> Result.withDefault Filter.dummy


exampleFilters : List Filter
exampleFilters =
    """[{"tag":"pathFilter","data":"a/path"},{"tag":"tagFilter","data":"a_tag"},{"tag":"fileFilter","data":"a/file.md"}]"""
        |> DecodeHelpers.runDecoder (TsDecode.list Filter.decoder)
        |> .decoded
        |> Result.withDefault []
