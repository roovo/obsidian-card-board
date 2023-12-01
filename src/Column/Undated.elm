module Column.Undated exposing
    ( UndatedColumn
    , addTaskItem
    , encoder
    , init
    , isCollapsed
    , isEnabled
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


type UndatedColumn
    = UndatedColumn Config (List String) TaskList


type alias Config =
    { collapsed : Bool
    , enabled : Bool
    , name : String
    }



-- CONSTRUCTION


init : String -> UndatedColumn
init name_ =
    UndatedColumn { collapsed = False, enabled = True, name = name_ } [] TaskList.empty



-- ENCODE


encoder : TsEncode.Encoder UndatedColumn
encoder =
    TsEncode.map config configEncoder



-- INFO


name : UndatedColumn -> String
name =
    .name << config


toList : UndatedColumn -> List TaskItem
toList (UndatedColumn _ _ tl) =
    tl
        |> TaskList.topLevelTasks
        |> List.sortBy (String.toLower << TaskItem.title)


isCollapsed : UndatedColumn -> Bool
isCollapsed =
    .collapsed << config


isEnabled : UndatedColumn -> Bool
isEnabled =
    .enabled << config


tagsToHide : UndatedColumn -> List String
tagsToHide (UndatedColumn _ tth _) =
    tth



-- MODIFICATION


addTaskItem : TaskItem -> UndatedColumn -> ( UndatedColumn, PlacementResult )
addTaskItem taskItem ((UndatedColumn c tth tl) as undatedColumn) =
    if not <| TaskItem.isDated taskItem then
        if TaskItem.isCompleted taskItem then
            ( undatedColumn, PlacementResult.CompletedInThisColumn )

        else
            ( UndatedColumn c tth (TaskList.add taskItem tl), PlacementResult.Placed )

    else
        ( undatedColumn, PlacementResult.DoesNotBelong )


setTagsToHide : List String -> UndatedColumn -> UndatedColumn
setTagsToHide tags (UndatedColumn c _ tl) =
    UndatedColumn c tags tl


toggleCollapse : UndatedColumn -> UndatedColumn
toggleCollapse (UndatedColumn c tth tl) =
    UndatedColumn { c | collapsed = not c.collapsed } tth tl


updateName : ColumnNames -> UndatedColumn -> UndatedColumn
updateName columnNames (UndatedColumn c tth tl) =
    UndatedColumn { c | name = ColumnNames.nameFor "undated" columnNames } tth tl



-- PRIVATE


config : UndatedColumn -> Config
config (UndatedColumn c _ _) =
    c


configEncoder : TsEncode.Encoder Config
configEncoder =
    TsEncode.object
        [ TsEncode.required "collapsed" .collapsed TsEncode.bool
        , TsEncode.required "enabled" .enabled TsEncode.bool
        , TsEncode.required "name" .name TsEncode.string
        ]
