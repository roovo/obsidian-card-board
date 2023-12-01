module Column.NamedTag exposing
    ( NamedTagColumn
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


type NamedTagColumn
    = NamedTagColumn Config (List String) TaskList


type alias Config =
    { collapsed : Bool
    , name : String
    , tag : String
    }



-- CONSTRUCTION


init : String -> String -> NamedTagColumn
init name_ tag_ =
    NamedTagColumn { collapsed = False, name = name_, tag = tag_ } [] TaskList.empty



-- ENCODE


encoder : TsEncode.Encoder NamedTagColumn
encoder =
    TsEncode.map config configEncoder



-- INFO


isCollapsed : NamedTagColumn -> Bool
isCollapsed (NamedTagColumn c _ _) =
    c.collapsed


name : NamedTagColumn -> String
name (NamedTagColumn c _ _) =
    c.name


toList : NamedTagColumn -> List TaskItem
toList (NamedTagColumn _ _ tl) =
    tl
        |> TaskList.topLevelTasks
        |> List.sortBy (String.toLower << TaskItem.title)
        |> List.sortBy TaskItem.dueRataDie


tagsToHide : NamedTagColumn -> List String
tagsToHide (NamedTagColumn _ tth _) =
    tth



-- MODIFICATION


addTaskItem : TaskItem -> NamedTagColumn -> ( NamedTagColumn, PlacementResult )
addTaskItem taskItem ((NamedTagColumn c tth tl) as namedTagColumn) =
    if belongs c.tag taskItem then
        if TaskItem.isCompleted taskItem then
            ( namedTagColumn, PlacementResult.CompletedInThisColumn )

        else
            ( NamedTagColumn c tth (TaskList.add taskItem tl), PlacementResult.Placed )

    else
        ( namedTagColumn, PlacementResult.DoesNotBelong )


setTagsToHide : List String -> NamedTagColumn -> NamedTagColumn
setTagsToHide tags (NamedTagColumn c _ tl) =
    NamedTagColumn c tags tl


toggleCollapse : NamedTagColumn -> NamedTagColumn
toggleCollapse (NamedTagColumn c tth tl) =
    NamedTagColumn { c | collapsed = not c.collapsed } tth tl


updateName : ColumnNames -> NamedTagColumn -> NamedTagColumn
updateName columnNames namedTagColumn =
    namedTagColumn



-- PRIVATE


belongs : String -> TaskItem -> Bool
belongs t =
    TaskItem.hasThisTag t


config : NamedTagColumn -> Config
config (NamedTagColumn c _ _) =
    c


configEncoder : TsEncode.Encoder Config
configEncoder =
    TsEncode.object
        [ TsEncode.required "collapsed" .collapsed TsEncode.bool
        , TsEncode.required "name" .name TsEncode.string
        , TsEncode.required "tag" .tag TsEncode.string
        ]
