module Column.OtherTags exposing
    ( OtherTagsColumn
    , addTaskItem
    , containsTask
    , decoder
    , encoder
    , init
    , isCollapsed
    , name
    , otherTags
    , setCollapse
    , setNameToDefault
    , setOtherTags
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


type OtherTagsColumn
    = OtherTagsColumn Config (List String) (List String) TaskList


type alias Config =
    { collapsed : Bool
    , name : String
    }



-- CONSTRUCTION


init : String -> List String -> OtherTagsColumn
init name_ otherTags_ =
    OtherTagsColumn { collapsed = False, name = name_ } otherTags_ [] TaskList.empty



-- DECODE / ENCODE


decoder : TsDecode.Decoder OtherTagsColumn
decoder =
    (TsDecode.succeed Config
        |> TsDecode.required "collapsed" TsDecode.bool
        |> TsDecode.required "name" TsDecode.string
    )
        |> TsDecode.map (\c -> OtherTagsColumn c [] [] TaskList.empty)


encoder : TsEncode.Encoder OtherTagsColumn
encoder =
    TsEncode.map config configEncoder



-- INFO


containsTask : String -> OtherTagsColumn -> Bool
containsTask taskId (OtherTagsColumn _ _ _ tl) =
    TaskList.containsTask taskId tl


isCollapsed : OtherTagsColumn -> Bool
isCollapsed (OtherTagsColumn c _ _ _) =
    c.collapsed


name : OtherTagsColumn -> String
name (OtherTagsColumn c _ _ _) =
    c.name


otherTags : OtherTagsColumn -> List String
otherTags (OtherTagsColumn _ ots _ _) =
    ots


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
addTaskItem taskItem ((OtherTagsColumn c ots tth tl) as otherTagsColumn) =
    let
        candidates : List TaskItem
        candidates =
            TaskItem.asSingleTaskItems taskItem
                |> List.filter TaskItem.hasTags
                |> List.map removeOtherColumnTags
                |> List.filter TaskItem.hasTags

        removeOtherColumnTags : TaskItem -> TaskItem
        removeOtherColumnTags item =
            let
                helper : String -> TaskItem -> TaskItem
                helper tag taskItem_ =
                    if TaskItem.hasThisTag tag taskItem_ then
                        TaskItem.removeMatchingTags tag taskItem_

                    else
                        taskItem_
            in
            List.foldl helper item ots
    in
    if List.length candidates == 0 then
        ( otherTagsColumn, PlacementResult.DoesNotBelong )

    else if
        TaskItem.isCompleted taskItem
            || List.all TaskItem.isCompleted candidates
    then
        ( otherTagsColumn, PlacementResult.CompletedInThisColumn )

    else
        ( OtherTagsColumn c ots tth (TaskList.add taskItem tl)
        , PlacementResult.Placed
        )


setCollapse : Bool -> OtherTagsColumn -> OtherTagsColumn
setCollapse isCollapsed_ (OtherTagsColumn c ots tth tl) =
    OtherTagsColumn { c | collapsed = isCollapsed_ } ots tth tl


setOtherTags : List String -> OtherTagsColumn -> OtherTagsColumn
setOtherTags tags (OtherTagsColumn c _ tth tl) =
    OtherTagsColumn c tags tth tl


setNameToDefault : DefaultColumnNames -> OtherTagsColumn -> OtherTagsColumn
setNameToDefault defaultColumnNames (OtherTagsColumn c ots tth tl) =
    OtherTagsColumn { c | name = DefaultColumnNames.nameFor "otherTags" defaultColumnNames } ots tth tl


setTagsToHide : List String -> OtherTagsColumn -> OtherTagsColumn
setTagsToHide tags (OtherTagsColumn c ots _ tl) =
    OtherTagsColumn c ots tags tl


toggleCollapse : OtherTagsColumn -> OtherTagsColumn
toggleCollapse (OtherTagsColumn c ots tth tl) =
    OtherTagsColumn { c | collapsed = not c.collapsed } ots tth tl


updateName : String -> OtherTagsColumn -> OtherTagsColumn
updateName newName (OtherTagsColumn c ots tth tl) =
    OtherTagsColumn { c | name = newName } ots tth tl



-- PRIVATE


config : OtherTagsColumn -> Config
config (OtherTagsColumn c _ _ _) =
    c


configEncoder : TsEncode.Encoder Config
configEncoder =
    TsEncode.object
        [ TsEncode.required "collapsed" .collapsed TsEncode.bool
        , TsEncode.required "name" .name TsEncode.string
        ]
