module Column.Completed exposing
    ( CompletedColumn
    , FormError(..)
    , LimitError(..)
    , addTaskItem
    , decoder
    , encoder
    , formDecoder
    , index
    , init
    , isCollapsed
    , limit
    , name
    , setCollapse
    , setIndex
    , setNameToDefault
    , setTagsToHide
    , tagsToHide
    , toList
    , toggleCollapse
    , updateCompletedCount
    , updateName
    )

import DefaultColumnNames exposing (DefaultColumnNames)
import Form.Decoder as FD
import PlacementResult exposing (PlacementResult)
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)
import TsJson.Decode as TsDecode
import TsJson.Decode.Pipeline as TsDecode
import TsJson.Encode as TsEncode



-- TYPES


type CompletedColumn
    = CompletedColumn Config (List String) TaskList


type alias Config =
    { collapsed : Bool
    , index : Int
    , limit : Int
    , name : String
    }


type alias Form =
    { name : String
    , limit : String
    }


type FormError
    = NameRequired
    | LimitError LimitError
    | LimitRequired


type LimitError
    = InvalidInt
    | Negative



-- CONSTRUCTION


init : String -> Int -> Int -> CompletedColumn
init name_ index_ limit_ =
    CompletedColumn { collapsed = False, index = index_, limit = limit_, name = name_ } [] TaskList.empty



-- DECODE / ENCODE


decoder : TsDecode.Decoder CompletedColumn
decoder =
    (TsDecode.succeed Config
        |> TsDecode.required "collapsed" TsDecode.bool
        |> TsDecode.required "index" TsDecode.int
        |> TsDecode.required "limit" TsDecode.int
        |> TsDecode.required "name" TsDecode.string
    )
        |> TsDecode.map (\c -> CompletedColumn c [] TaskList.empty)


encoder : TsEncode.Encoder CompletedColumn
encoder =
    TsEncode.map config configEncoder


formDecoder : FD.Decoder Form FormError CompletedColumn
formDecoder =
    FD.map2 fromForm
        formDecoderName
        formDecoderLimit



-- INFO


index : CompletedColumn -> Int
index (CompletedColumn c _ _) =
    c.index


isCollapsed : CompletedColumn -> Bool
isCollapsed (CompletedColumn c _ _) =
    c.collapsed


limit : CompletedColumn -> Int
limit (CompletedColumn c _ _) =
    c.limit


name : CompletedColumn -> String
name (CompletedColumn c _ _) =
    c.name


tagsToHide : CompletedColumn -> List String
tagsToHide (CompletedColumn _ tth _) =
    tth


toList : CompletedColumn -> List TaskItem
toList (CompletedColumn c _ tl) =
    tl
        |> TaskList.topLevelTasks
        |> List.sortBy (String.toLower << TaskItem.title)
        |> List.reverse
        |> List.sortBy TaskItem.completedPosix
        |> List.reverse
        |> List.take c.limit



-- MANIPULATION


addTaskItem : List PlacementResult -> TaskItem -> CompletedColumn -> CompletedColumn
addTaskItem placementResults taskItem ((CompletedColumn c tth tl) as completedColumn) =
    let
        filteredPlacements : List PlacementResult
        filteredPlacements =
            placementResults
                |> List.filter (\r -> r /= PlacementResult.DoesNotBelong)

        shouldBeAdded : Bool
        shouldBeAdded =
            filteredPlacements
                |> List.all (\r -> r == PlacementResult.CompletedInThisColumn)
                |> (&&) (not <| List.isEmpty filteredPlacements)
    in
    if shouldBeAdded then
        CompletedColumn c tth (TaskList.add taskItem tl)

    else
        completedColumn


setCollapse : Bool -> CompletedColumn -> CompletedColumn
setCollapse isCollapsed_ (CompletedColumn c tth tl) =
    CompletedColumn { c | collapsed = isCollapsed_ } tth tl


setIndex : Int -> CompletedColumn -> CompletedColumn
setIndex newIndex (CompletedColumn c tth tl) =
    CompletedColumn { c | index = newIndex } tth tl


setNameToDefault : DefaultColumnNames -> CompletedColumn -> CompletedColumn
setNameToDefault defaultColumnNames (CompletedColumn c tth tl) =
    CompletedColumn { c | name = DefaultColumnNames.nameFor "completed" defaultColumnNames } tth tl


setTagsToHide : List String -> CompletedColumn -> CompletedColumn
setTagsToHide tags (CompletedColumn c _ tl) =
    CompletedColumn c tags tl


toggleCollapse : CompletedColumn -> CompletedColumn
toggleCollapse (CompletedColumn c tth tl) =
    CompletedColumn { c | collapsed = not c.collapsed } tth tl


updateCompletedCount : Int -> CompletedColumn -> CompletedColumn
updateCompletedCount newCount (CompletedColumn c tth tl) =
    CompletedColumn { c | limit = newCount } tth tl


updateName : String -> CompletedColumn -> CompletedColumn
updateName newName (CompletedColumn c tth tl) =
    CompletedColumn { c | name = newName } tth tl



-- PRIVATE


config : CompletedColumn -> Config
config (CompletedColumn c _ _) =
    c


configEncoder : TsEncode.Encoder Config
configEncoder =
    TsEncode.object
        [ TsEncode.required "collapsed" .collapsed TsEncode.bool
        , TsEncode.required "index" .index TsEncode.int
        , TsEncode.required "limit" .limit TsEncode.int
        , TsEncode.required "name" .name TsEncode.string
        ]


formDecoderLimit : FD.Decoder Form FormError Int
formDecoderLimit =
    FD.int InvalidInt
        |> FD.assert (FD.minBound Negative 0)
        |> FD.mapError LimitError
        |> required LimitRequired
        |> FD.lift .limit


formDecoderName : FD.Decoder Form FormError String
formDecoderName =
    FD.identity
        |> required NameRequired
        |> FD.lift .name


fromForm : String -> Int -> CompletedColumn
fromForm name_ limit_ =
    init name_ 0 limit_


required : err -> FD.Decoder String err a -> FD.Decoder String err a
required error d =
    FD.with <|
        \a ->
            case a of
                "" ->
                    FD.fail error

                _ ->
                    FD.lift identity d
