module Column.OtherTags exposing
    ( OtherTagsColumn
    , addTaskItem
    , decoder
    , disable
    , enable
    , encoder
    , init
    , isCollapsed
    , isEnabled
    , name
    , setCollapse
    , setNameToDefault
    , setOtherTags
    , setTagsToHide
    , tagsToHide
    , toList
    , toggleCollapse
    )

import DecodeHelpers
import DefaultColumnNames exposing (DefaultColumnNames)
import PlacementResult exposing (PlacementResult)
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)
import TsJson.Decode as TsDecode
import TsJson.Decode.Pipeline as TsDecode
import TsJson.Encode as TsEncode



-- TYPES


type OtherTagsColumn
    = OtherTagsColumn Config (List String) (List String) TaskList


type alias Config =
    { collapsed : Bool
    , enabled : Bool
    , name : String
    }



-- CONSTRUCTION


init : String -> List String -> OtherTagsColumn
init name_ otherTags_ =
    OtherTagsColumn { collapsed = False, enabled = True, name = name_ } otherTags_ [] TaskList.empty



-- DECODE / ENCODE


decoder : TsDecode.Decoder OtherTagsColumn
decoder =
    (TsDecode.succeed Config
        |> TsDecode.required "collapsed" TsDecode.bool
        |> TsDecode.required "enabled" TsDecode.bool
        |> TsDecode.required "name" TsDecode.string
    )
        |> TsDecode.map (\c -> OtherTagsColumn c [] [] TaskList.empty)


encoder : TsEncode.Encoder OtherTagsColumn
encoder =
    TsEncode.map config configEncoder



-- INFO


isCollapsed : OtherTagsColumn -> Bool
isCollapsed (OtherTagsColumn c _ _ _) =
    c.collapsed


isEnabled : OtherTagsColumn -> Bool
isEnabled (OtherTagsColumn c _ _ _) =
    c.enabled


name : OtherTagsColumn -> String
name (OtherTagsColumn c _ _ _) =
    c.name


toList : OtherTagsColumn -> List TaskItem
toList (OtherTagsColumn _ _ _ tl) =
    tl
        |> TaskList.topLevelTasks
        |> List.sortBy (String.toLower << TaskItem.title)
        |> List.sortBy TaskItem.dueRataDie


tagsToHide : OtherTagsColumn -> List String
tagsToHide (OtherTagsColumn _ _ tth _) =
    tth



-- MODIFICATION


addTaskItem : TaskItem -> OtherTagsColumn -> ( OtherTagsColumn, PlacementResult )
addTaskItem taskItem ((OtherTagsColumn c ots tth tl) as namedTagColumn) =
    if isEnabled namedTagColumn && belongs ots taskItem then
        if isCompleted ots taskItem then
            ( namedTagColumn, PlacementResult.CompletedInThisColumn )

        else
            ( OtherTagsColumn c ots tth (TaskList.add taskItem tl), PlacementResult.Placed )

    else
        ( namedTagColumn, PlacementResult.DoesNotBelong )


disable : OtherTagsColumn -> OtherTagsColumn
disable (OtherTagsColumn c ots tth tl) =
    OtherTagsColumn { c | enabled = False } ots tth tl


enable : OtherTagsColumn -> OtherTagsColumn
enable (OtherTagsColumn c ots tth tl) =
    OtherTagsColumn { c | enabled = True } ots tth tl


setCollapse : Bool -> OtherTagsColumn -> OtherTagsColumn
setCollapse isCollapsed_ (OtherTagsColumn c ots tth tl) =
    OtherTagsColumn { c | collapsed = isCollapsed_ } ots tth tl


setOtherTags : List String -> OtherTagsColumn -> OtherTagsColumn
setOtherTags tags (OtherTagsColumn c ots tth tl) =
    OtherTagsColumn c tags tth tl


setTagsToHide : List String -> OtherTagsColumn -> OtherTagsColumn
setTagsToHide tags (OtherTagsColumn c ots _ tl) =
    OtherTagsColumn c ots tags tl


toggleCollapse : OtherTagsColumn -> OtherTagsColumn
toggleCollapse (OtherTagsColumn c ots tth tl) =
    OtherTagsColumn { c | collapsed = not c.collapsed } ots tth tl


setNameToDefault : DefaultColumnNames -> OtherTagsColumn -> OtherTagsColumn
setNameToDefault defaultColumnNames (OtherTagsColumn c ots tth tl) =
    OtherTagsColumn { c | name = DefaultColumnNames.nameFor "others" defaultColumnNames } ots tth tl



-- PRIVATE


belongs : List String -> TaskItem -> Bool
belongs tags item =
    TaskItem.hasTaskWithTagOtherThanThese tags item


config : OtherTagsColumn -> Config
config (OtherTagsColumn c _ _ _) =
    c


configEncoder : TsEncode.Encoder Config
configEncoder =
    TsEncode.object
        [ TsEncode.required "collapsed" .collapsed TsEncode.bool
        , TsEncode.required "enabled" .enabled TsEncode.bool
        , TsEncode.required "name" .name TsEncode.string
        ]


isCompleted : List String -> TaskItem -> Bool
isCompleted columnTags taskItem =
    TaskItem.isCompleted taskItem
        || (TaskItem.hasSubtasks taskItem
                && not (TaskItem.hasAnyIncompleteSubtasksWithTagsOtherThanThese columnTags taskItem)
           )
