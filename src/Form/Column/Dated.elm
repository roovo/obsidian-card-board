module Form.Column.Dated exposing
    ( DatedColumnForm
    , init
    , safeDecoder
    , updateFrom
    , updateName
    , updateRangeType
    , updateTo
    )

import Column.Dated as DatedColumn exposing (DatedColumn, RelativeDateRange)
import Form.SafeDecoder as SD



-- TYPES


type alias DatedColumnForm =
    { from : String
    , name : String
    , rangeType : String
    , to : String
    }


type RangeType
    = After
    | Before
    | Between


type alias Range =
    { from : Int
    , to : Int
    }



-- CONSTRUCTION


init : DatedColumn -> DatedColumnForm
init datedColumn =
    let
        rangeType : String
        rangeType =
            case DatedColumn.range datedColumn of
                DatedColumn.After _ ->
                    "After"

                DatedColumn.Before _ ->
                    "Before"

                DatedColumn.Between _ ->
                    "Between"

        from : String
        from =
            case DatedColumn.range datedColumn of
                DatedColumn.After from_ ->
                    String.fromInt from_

                DatedColumn.Before _ ->
                    ""

                DatedColumn.Between fromTo ->
                    String.fromInt fromTo.from

        to : String
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


safeDecoder : SD.Decoder DatedColumnForm DatedColumn
safeDecoder =
    SD.map2 DatedColumn.init
        safeNameDecoder
        safeRangeDecoder



-- MODIFICATION


updateFrom : String -> DatedColumnForm -> DatedColumnForm
updateFrom newValue form =
    { form | from = newValue }


updateName : String -> DatedColumnForm -> DatedColumnForm
updateName newName form =
    { form | name = newName }


updateRangeType : String -> DatedColumnForm -> DatedColumnForm
updateRangeType newType form =
    { form | rangeType = newType }


updateTo : String -> DatedColumnForm -> DatedColumnForm
updateTo newValue form =
    { form | to = newValue }



-- PRIVATE


safeRangeToValueDecoder : SD.Decoder DatedColumnForm Int
safeRangeToValueDecoder =
    SD.int 0
        |> SD.lift String.trim
        |> SD.lift .to


safeRangeFromValueDecoder : SD.Decoder DatedColumnForm Int
safeRangeFromValueDecoder =
    SD.int 0
        |> SD.lift String.trim
        |> SD.lift .from


safeRangeBetweenValueDecoder : SD.Decoder DatedColumnForm Range
safeRangeBetweenValueDecoder =
    SD.map2 Range
        safeRangeFromValueDecoder
        safeRangeToValueDecoder


safeNameDecoder : SD.Decoder DatedColumnForm String
safeNameDecoder =
    SD.identity
        |> SD.lift String.trim
        |> SD.lift .name


safeRangeDecoder : SD.Decoder DatedColumnForm RelativeDateRange
safeRangeDecoder =
    safeRangeTypeDecoder
        |> SD.lift .rangeType
        |> SD.andThen safeRangeDecoder_


safeRangeDecoder_ : RangeType -> SD.Decoder DatedColumnForm RelativeDateRange
safeRangeDecoder_ rangeType =
    case rangeType of
        After ->
            SD.map DatedColumn.After safeRangeFromValueDecoder

        Before ->
            SD.map DatedColumn.Before safeRangeToValueDecoder

        Between ->
            SD.map DatedColumn.Between safeRangeBetweenValueDecoder


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
