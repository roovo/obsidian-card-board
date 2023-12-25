module Form.DatedColumn exposing
    ( Error(..)
    , Form
    , RangeTypeError(..)
    , RangeValueError(..)
    , decoder
    , init
    , safeDecoder
    , updateFrom
    , updateName
    , updateRangeType
    , updateTo
    )

import Column.Dated as DatedColumn exposing (DatedColumn, RelativeDateRange)
import DecodeHelpers
import DefaultColumnNames exposing (DefaultColumnNames)
import Form.Decoder as FD
import Form.Input as Input
import Form.SafeDecoder as SD
import PlacementResult exposing (PlacementResult)
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)
import TsJson.Decode as TsDecode
import TsJson.Decode.Pipeline as TsDecode
import TsJson.Encode as TsEncode



-- TYPES


type alias Form =
    { from : String
    , name : String
    , rangeType : String
    , to : String
    }


type Error
    = NameRequired
    | RangeFromValueRequired
    | RangeToValueRequired
    | RangeTypeError RangeTypeError
    | RangeTypeRequired
    | RangeValueFromError RangeValueError
    | RangeValueToError RangeValueError


type RangeTypeError
    = Invalid


type RangeValueError
    = InvalidInt


type RangeType
    = After
    | Before
    | Between


type alias Range =
    { from : Int
    , to : Int
    }



-- CONSTRUCTION


init : DatedColumn -> Form
init datedColumn =
    let
        rangeType =
            case DatedColumn.range datedColumn of
                DatedColumn.After _ ->
                    "After"

                DatedColumn.Before _ ->
                    "Before"

                DatedColumn.Between _ ->
                    "Between"

        from =
            case DatedColumn.range datedColumn of
                DatedColumn.After from_ ->
                    String.fromInt from_

                DatedColumn.Before _ ->
                    ""

                DatedColumn.Between fromTo ->
                    String.fromInt fromTo.from

        to =
            case DatedColumn.range datedColumn of
                DatedColumn.After _ ->
                    ""

                DatedColumn.Before to_ ->
                    String.fromInt to_

                DatedColumn.Between fromTo ->
                    String.fromInt fromTo.to
    in
    { from = from
    , name = DatedColumn.name datedColumn
    , rangeType = rangeType
    , to = to
    }



-- DECODE


decoder : FD.Decoder Form Error DatedColumn
decoder =
    FD.map2 DatedColumn.init
        nameDecoder
        rangeDecoder


safeDecoder : SD.Decoder Form DatedColumn
safeDecoder =
    SD.map2 DatedColumn.init
        safeNameDecoder
        safeRangeDecoder



-- MODIFICATION


updateFrom : String -> Form -> Form
updateFrom newValue form =
    { form | from = newValue }


updateName : String -> Form -> Form
updateName newName form =
    { form | name = newName }


updateRangeType : String -> Form -> Form
updateRangeType newType form =
    { form | rangeType = newType }


updateTo : String -> Form -> Form
updateTo newValue form =
    { form | to = newValue }



-- PRIVATE


formRangeBetweenValueDecoder : FD.Decoder Form Error Range
formRangeBetweenValueDecoder =
    FD.map2 Range
        formRangeFromValueDecoder
        formRangeToValueDecoder


formRangeFromValueDecoder : FD.Decoder Form Error Int
formRangeFromValueDecoder =
    FD.int InvalidInt
        |> FD.lift String.trim
        |> FD.mapError RangeValueFromError
        |> Input.required RangeFromValueRequired
        |> FD.lift .from


formRangeToValueDecoder : FD.Decoder Form Error Int
formRangeToValueDecoder =
    FD.int InvalidInt
        |> FD.lift String.trim
        |> FD.mapError RangeValueToError
        |> Input.required RangeToValueRequired
        |> FD.lift .to


nameDecoder : FD.Decoder Form Error String
nameDecoder =
    FD.identity
        |> FD.lift String.trim
        |> Input.required NameRequired
        |> FD.lift .name


rangeDecoder : FD.Decoder Form Error RelativeDateRange
rangeDecoder =
    rangeTypeDecoder
        |> FD.mapError RangeTypeError
        |> Input.required RangeTypeRequired
        |> FD.lift .rangeType
        |> FD.andThen rangeDecoder_


rangeDecoder_ : RangeType -> FD.Decoder Form Error RelativeDateRange
rangeDecoder_ rangeType =
    case rangeType of
        After ->
            FD.map DatedColumn.After formRangeFromValueDecoder

        Before ->
            FD.map DatedColumn.Before formRangeToValueDecoder

        Between ->
            FD.map DatedColumn.Between formRangeBetweenValueDecoder


rangeTypeDecoder : FD.Decoder String RangeTypeError RangeType
rangeTypeDecoder =
    FD.custom <|
        \str ->
            case String.trim str of
                "After" ->
                    Ok After

                "Before" ->
                    Ok Before

                "Between" ->
                    Ok Between

                _ ->
                    Err [ Invalid ]


safeFormRangeToValueDecoder : SD.Decoder Form Int
safeFormRangeToValueDecoder =
    SD.int 0
        |> SD.lift String.trim
        |> SD.lift .to


safeFormRangeFromValueDecoder : SD.Decoder Form Int
safeFormRangeFromValueDecoder =
    SD.int 0
        |> SD.lift String.trim
        |> SD.lift .from


safeFormRangeBetweenValueDecoder : SD.Decoder Form Range
safeFormRangeBetweenValueDecoder =
    SD.map2 Range
        safeFormRangeFromValueDecoder
        safeFormRangeToValueDecoder


safeNameDecoder : SD.Decoder Form String
safeNameDecoder =
    SD.identity
        |> SD.lift String.trim
        |> SD.lift .name


safeRangeDecoder : SD.Decoder Form RelativeDateRange
safeRangeDecoder =
    safeRangeTypeDecoder
        |> SD.lift .rangeType
        |> SD.andThen safeRangeDecoder_


safeRangeDecoder_ : RangeType -> SD.Decoder Form RelativeDateRange
safeRangeDecoder_ rangeType =
    case rangeType of
        After ->
            SD.map DatedColumn.After safeFormRangeFromValueDecoder

        Before ->
            SD.map DatedColumn.Before safeFormRangeToValueDecoder

        Between ->
            SD.map DatedColumn.Between safeFormRangeBetweenValueDecoder


safeRangeTypeDecoder : SD.Decoder String RangeType
safeRangeTypeDecoder =
    SD.custom <|
        \str ->
            case String.trim str of
                "After" ->
                    Ok After

                "Before" ->
                    Ok Before

                "Between" ->
                    Ok Between

                _ ->
                    Ok Before
