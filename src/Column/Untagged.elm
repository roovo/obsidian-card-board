module Column.Untagged exposing
    ( UntaggedColumn
    , addTaskItem
    , encoder
    , init
    , isCollapsed
    , name
    , setTagsToHide
    , tagsToHide
    , toList
    , toggleCollapse
    , updateName
    )

import ColumnNames exposing (ColumnNames)
import PlacementResult exposing (PlacementResult)
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)
import TsJson.Encode as TsEncode



-- TYPES


type UntaggedColumn
    = UntaggedColumn Config (List String) TaskList


type alias Config =
    { collapsed : Bool
    , enabled : Bool
    , name : String
    }



-- CONSTRUCTION


init : String -> UntaggedColumn
init name_ =
    UntaggedColumn { collapsed = False, enabled = True, name = name_ } [] TaskList.empty



-- ENCODE


encoder : TsEncode.Encoder UntaggedColumn
encoder =
    TsEncode.map config configEncoder



-- INFO


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
    if not <| TaskItem.isDated taskItem then
        if TaskItem.isCompleted taskItem then
            ( untaggedColumn, PlacementResult.CompletedInThisColumn )

        else
            ( UntaggedColumn c tth (TaskList.add taskItem tl), PlacementResult.Placed )

    else
        ( untaggedColumn, PlacementResult.DoesNotBelong )


setTagsToHide : List String -> UntaggedColumn -> UntaggedColumn
setTagsToHide tags (UntaggedColumn c _ tl) =
    UntaggedColumn c tags tl


toggleCollapse : UntaggedColumn -> UntaggedColumn
toggleCollapse (UntaggedColumn c tth tl) =
    UntaggedColumn { c | collapsed = not c.collapsed } tth tl


updateName : ColumnNames -> UntaggedColumn -> UntaggedColumn
updateName columnNames (UntaggedColumn c tth tl) =
    UntaggedColumn { c | name = ColumnNames.nameFor "untagged" columnNames } tth tl



-- PRIVATE


config : UntaggedColumn -> Config
config (UntaggedColumn c _ _) =
    c


configEncoder : TsEncode.Encoder Config
configEncoder =
    TsEncode.object
        [ TsEncode.required "collapsed" .collapsed TsEncode.bool
        , TsEncode.required "enabled" .enabled TsEncode.bool
        , TsEncode.required "name" .name TsEncode.string
        ]
