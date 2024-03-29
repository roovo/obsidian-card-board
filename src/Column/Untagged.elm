module Column.Untagged exposing
    ( UntaggedColumn
    , addTaskItem
    , containsTask
    , decoder
    , encoder
    , init
    , isCollapsed
    , name
    , setCollapse
    , setNameToDefault
    , setTagsToHide
    , tagsToHide
    , toList
    , toggleCollapse
    , updateName
    )

import DefaultColumnNames exposing (DefaultColumnNames)
import PlacementResult exposing (PlacementResult)
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)
import TsJson.Decode as TsDecode
import TsJson.Decode.Pipeline as TsDecode
import TsJson.Encode as TsEncode



-- TYPES


type UntaggedColumn
    = UntaggedColumn Config (List String) TaskList


type alias Config =
    { collapsed : Bool
    , name : String
    }



-- CONSTRUCTION


init : String -> UntaggedColumn
init name_ =
    UntaggedColumn { collapsed = False, name = name_ } [] TaskList.empty



-- DECODE / ENCODE


decoder : TsDecode.Decoder UntaggedColumn
decoder =
    (TsDecode.succeed Config
        |> TsDecode.required "collapsed" TsDecode.bool
        |> TsDecode.required "name" TsDecode.string
    )
        |> TsDecode.map (\c -> UntaggedColumn c [] TaskList.empty)


encoder : TsEncode.Encoder UntaggedColumn
encoder =
    TsEncode.map config configEncoder



-- INFO


containsTask : String -> UntaggedColumn -> Bool
containsTask taskId (UntaggedColumn _ _ tl) =
    TaskList.containsTask taskId tl


name : UntaggedColumn -> String
name =
    .name << config


toList : UntaggedColumn -> List TaskItem
toList (UntaggedColumn _ _ tl) =
    tl
        |> TaskList.topLevelTasks
        |> List.sortBy (String.toLower << TaskItem.title)
        |> List.sortBy TaskItem.dueRataDie


isCollapsed : UntaggedColumn -> Bool
isCollapsed =
    .collapsed << config


tagsToHide : UntaggedColumn -> List String
tagsToHide (UntaggedColumn _ tth _) =
    tth



-- MODIFICATION


addTaskItem : TaskItem -> UntaggedColumn -> ( UntaggedColumn, PlacementResult )
addTaskItem taskItem ((UntaggedColumn c tth tl) as untaggedColumn) =
    if not <| TaskItem.hasTags taskItem then
        if TaskItem.isCompleted taskItem then
            ( untaggedColumn, PlacementResult.CompletedInThisColumn )

        else
            ( UntaggedColumn c tth (TaskList.add taskItem tl), PlacementResult.Placed )

    else
        ( untaggedColumn, PlacementResult.DoesNotBelong )


setCollapse : Bool -> UntaggedColumn -> UntaggedColumn
setCollapse isCollapsed_ (UntaggedColumn c tth tl) =
    UntaggedColumn { c | collapsed = isCollapsed_ } tth tl


setNameToDefault : DefaultColumnNames -> UntaggedColumn -> UntaggedColumn
setNameToDefault defaultColumnNames (UntaggedColumn c tth tl) =
    UntaggedColumn { c | name = DefaultColumnNames.nameFor "untagged" defaultColumnNames } tth tl


setTagsToHide : List String -> UntaggedColumn -> UntaggedColumn
setTagsToHide tags (UntaggedColumn c _ tl) =
    UntaggedColumn c tags tl


toggleCollapse : UntaggedColumn -> UntaggedColumn
toggleCollapse (UntaggedColumn c tth tl) =
    UntaggedColumn { c | collapsed = not c.collapsed } tth tl


updateName : String -> UntaggedColumn -> UntaggedColumn
updateName newName (UntaggedColumn c tth tl) =
    UntaggedColumn { c | name = newName } tth tl



-- PRIVATE


config : UntaggedColumn -> Config
config (UntaggedColumn c _ _) =
    c


configEncoder : TsEncode.Encoder Config
configEncoder =
    TsEncode.object
        [ TsEncode.required "collapsed" .collapsed TsEncode.bool
        , TsEncode.required "name" .name TsEncode.string
        ]
