module Column.Dated exposing
    ( DatedColumn
    , RelativeDateRange(..)
    , addTaskItem
    , decoder
    , encoder
    , init
    , isCollapsed
    , isEnabled
    , name
    , range
    , setCollapse
    , setNameToDefault
    , setTagsToHide
    , tagsToHide
    , toList
    , toggleCollapse
    , updateName
    , updateRangeType
    , updateRangeValueFrom
    , updateRangeValueTo
    )

import Date exposing (Date)
import DecodeHelpers
import DefaultColumnNames exposing (DefaultColumnNames)
import PlacementResult exposing (PlacementResult)
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)
import TsJson.Decode as TsDecode
import TsJson.Decode.Pipeline as TsDecode
import TsJson.Encode as TsEncode



-- TYPES


type DatedColumn
    = DatedColumn Config (List String) TaskList


type alias Config =
    { collapsed : Bool
    , name : String
    , range : RelativeDateRange
    }


type RelativeDateRange
    = After Int
    | Before Int
    | Between { from : Int, to : Int }



-- CONSTRUCTION


init : String -> RelativeDateRange -> DatedColumn
init name_ range_ =
    DatedColumn { collapsed = False, name = name_, range = range_ } [] TaskList.empty



-- DECODE / ENCODE


decoder : TsDecode.Decoder DatedColumn
decoder =
    (TsDecode.succeed Config
        |> TsDecode.required "collapsed" TsDecode.bool
        |> TsDecode.required "name" TsDecode.string
        |> TsDecode.required "range" relativeDateRangeDecoder
    )
        |> TsDecode.map (\c -> DatedColumn c [] TaskList.empty)


encoder : TsEncode.Encoder DatedColumn
encoder =
    TsEncode.map config configEncoder



-- INFO


isCollapsed : DatedColumn -> Bool
isCollapsed (DatedColumn c _ _) =
    c.collapsed


isEnabled : DatedColumn -> Bool
isEnabled _ =
    True


name : DatedColumn -> String
name (DatedColumn c _ _) =
    c.name


range : DatedColumn -> RelativeDateRange
range (DatedColumn c _ _) =
    c.range


tagsToHide : DatedColumn -> List String
tagsToHide (DatedColumn _ tth _) =
    tth


toList : DatedColumn -> List TaskItem
toList (DatedColumn _ _ tl) =
    tl
        |> TaskList.topLevelTasks
        |> List.sortBy (String.toLower << TaskItem.title)
        |> List.sortBy TaskItem.dueRataDie



-- MODIFICATION


addTaskItem : Date -> TaskItem -> DatedColumn -> ( DatedColumn, PlacementResult )
addTaskItem today taskItem ((DatedColumn c tth tl) as datedColumn) =
    case TaskItem.due taskItem of
        Just dueDate ->
            if belongs today c.range dueDate then
                if TaskItem.isCompleted taskItem then
                    ( datedColumn, PlacementResult.CompletedInThisColumn )

                else
                    ( DatedColumn c tth (TaskList.add taskItem tl), PlacementResult.Placed )

            else
                ( datedColumn, PlacementResult.DoesNotBelong )

        Nothing ->
            ( datedColumn, PlacementResult.DoesNotBelong )


setCollapse : Bool -> DatedColumn -> DatedColumn
setCollapse isCollapsed_ (DatedColumn c tth tl) =
    DatedColumn { c | collapsed = isCollapsed_ } tth tl


setNameToDefault : DefaultColumnNames -> DatedColumn -> DatedColumn
setNameToDefault defaultColumnNames (DatedColumn c tth tl) =
    let
        newName =
            case c.name of
                "Today" ->
                    DefaultColumnNames.nameFor "today" defaultColumnNames

                "Tomorrow" ->
                    DefaultColumnNames.nameFor "tomorrow" defaultColumnNames

                "Future" ->
                    DefaultColumnNames.nameFor "future" defaultColumnNames

                _ ->
                    c.name
    in
    DatedColumn { c | name = newName } tth tl


setTagsToHide : List String -> DatedColumn -> DatedColumn
setTagsToHide tags (DatedColumn c _ tl) =
    DatedColumn c tags tl


toggleCollapse : DatedColumn -> DatedColumn
toggleCollapse (DatedColumn c tth tl) =
    DatedColumn { c | collapsed = not c.collapsed } tth tl


updateName : String -> DatedColumn -> DatedColumn
updateName newName (DatedColumn c tth tl) =
    DatedColumn { c | name = newName } tth tl


updateRangeType : String -> DatedColumn -> DatedColumn
updateRangeType rangeType ((DatedColumn c tth tl) as datedColumn) =
    case ( c.range, rangeType ) of
        ( Before to, "After" ) ->
            DatedColumn { c | range = After to } tth tl

        ( Before to, "Between" ) ->
            DatedColumn { c | range = Between { from = to, to = to } } tth tl

        ( After from, "Before" ) ->
            DatedColumn { c | range = Before from } tth tl

        ( After from, "Between" ) ->
            DatedColumn { c | range = Between { from = from, to = from } } tth tl

        ( Between fromTo, "Before" ) ->
            DatedColumn { c | range = Before (min fromTo.from fromTo.to) } tth tl

        ( Between fromTo, "After" ) ->
            DatedColumn { c | range = After (max fromTo.from fromTo.to) } tth tl

        _ ->
            datedColumn


updateRangeValueFrom : Int -> DatedColumn -> DatedColumn
updateRangeValueFrom newValue ((DatedColumn c tth tl) as datedColumn) =
    case c.range of
        Before value ->
            DatedColumn { c | range = Before value } tth tl

        After value ->
            DatedColumn { c | range = After newValue } tth tl

        Between fromTo ->
            DatedColumn { c | range = Between { from = newValue, to = fromTo.to } } tth tl


updateRangeValueTo : Int -> DatedColumn -> DatedColumn
updateRangeValueTo newValue ((DatedColumn c tth tl) as datedColumn) =
    case c.range of
        Before value ->
            DatedColumn { c | range = Before newValue } tth tl

        After value ->
            DatedColumn { c | range = After value } tth tl

        Between fromTo ->
            DatedColumn { c | range = Between { from = fromTo.from, to = newValue } } tth tl



-- PRIVATE


belongs : Date -> RelativeDateRange -> Date -> Bool
belongs today range_ taskDate =
    case range_ of
        Between values ->
            Date.isBetween
                (Date.add Date.Days (min values.from values.to) today)
                (Date.add Date.Days (max values.from values.to) today)
                taskDate

        Before to ->
            Date.diff Date.Days taskDate (Date.add Date.Days (to - 1) today) >= 0

        After from ->
            Date.diff Date.Days (Date.add Date.Days (from + 1) today) taskDate >= 0


config : DatedColumn -> Config
config (DatedColumn c _ _) =
    c


configEncoder : TsEncode.Encoder Config
configEncoder =
    TsEncode.object
        [ TsEncode.required "collapsed" .collapsed TsEncode.bool
        , TsEncode.required "name" .name TsEncode.string
        , TsEncode.required "range" .range relativeDateRangeEncoder
        ]


relativeDateRangeDecoder : TsDecode.Decoder RelativeDateRange
relativeDateRangeDecoder =
    TsDecode.oneOf
        [ DecodeHelpers.toElmVariant "after" After TsDecode.int
        , DecodeHelpers.toElmVariant "before" Before TsDecode.int
        , DecodeHelpers.toElmVariant "between" Between rangeDecoder
        ]


relativeDateRangeEncoder : TsEncode.Encoder RelativeDateRange
relativeDateRangeEncoder =
    TsEncode.union
        (\vAfter vBefore vBetween value ->
            case value of
                After from ->
                    vAfter from

                Before to ->
                    vBefore to

                Between range_ ->
                    vBetween range_
        )
        |> TsEncode.variantTagged "after" TsEncode.int
        |> TsEncode.variantTagged "before" TsEncode.int
        |> TsEncode.variantTagged "between" rangeEncoder
        |> TsEncode.buildUnion


rangeDecoder : TsDecode.Decoder { from : Int, to : Int }
rangeDecoder =
    TsDecode.succeed (\f -> \t -> { from = f, to = t })
        |> TsDecode.required "from" TsDecode.int
        |> TsDecode.required "to" TsDecode.int


rangeEncoder : TsEncode.Encoder { from : Int, to : Int }
rangeEncoder =
    TsEncode.object
        [ TsEncode.required "from" .from TsEncode.int
        , TsEncode.required "to" .to TsEncode.int
        ]
