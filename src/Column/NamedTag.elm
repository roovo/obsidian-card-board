module Column.NamedTag exposing
    ( NamedTagColumn
    , addTaskItem
    , asInputString
    , decoder
    , encoder
    , init
    , isCollapsed
    , isEnabled
    , name
    , setCollapse
    , setTagsToHide
    , tag
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



-- DECODE / ENCODE


decoder : TsDecode.Decoder NamedTagColumn
decoder =
    (TsDecode.succeed Config
        |> TsDecode.required "collapsed" TsDecode.bool
        |> TsDecode.required "name" TsDecode.string
        |> TsDecode.required "tag" TsDecode.string
    )
        |> TsDecode.map (\c -> NamedTagColumn c [] TaskList.empty)


encoder : TsEncode.Encoder NamedTagColumn
encoder =
    TsEncode.map config configEncoder



-- INFO


asInputString : NamedTagColumn -> String
asInputString (NamedTagColumn c _ _) =
    "#" ++ c.tag ++ " " ++ c.name


isCollapsed : NamedTagColumn -> Bool
isCollapsed (NamedTagColumn c _ _) =
    c.collapsed


isEnabled : NamedTagColumn -> Bool
isEnabled _ =
    True


name : NamedTagColumn -> String
name (NamedTagColumn c _ _) =
    c.name


tag : NamedTagColumn -> String
tag (NamedTagColumn c _ _) =
    c.tag


tagsToHide : NamedTagColumn -> List String
tagsToHide (NamedTagColumn _ tth _) =
    tth


toList : NamedTagColumn -> List TaskItem
toList (NamedTagColumn _ _ tl) =
    tl
        |> TaskList.topLevelTasks
        |> List.sortBy (String.toLower << TaskItem.title)
        |> List.sortBy TaskItem.dueRataDie



-- MODIFICATION


addTaskItem : TaskItem -> NamedTagColumn -> ( NamedTagColumn, PlacementResult )
addTaskItem taskItem ((NamedTagColumn c tth tl) as namedTagColumn) =
    if belongs c.tag taskItem then
        if isCompleted c.tag taskItem then
            ( namedTagColumn, PlacementResult.CompletedInThisColumn )

        else
            ( NamedTagColumn c tth (TaskList.add taskItem tl), PlacementResult.Placed )

    else
        ( namedTagColumn, PlacementResult.DoesNotBelong )


setCollapse : Bool -> NamedTagColumn -> NamedTagColumn
setCollapse isCollapsed_ (NamedTagColumn c tth tl) =
    NamedTagColumn { c | collapsed = isCollapsed_ } tth tl


setTagsToHide : List String -> NamedTagColumn -> NamedTagColumn
setTagsToHide tags (NamedTagColumn c _ tl) =
    NamedTagColumn c tags tl


toggleCollapse : NamedTagColumn -> NamedTagColumn
toggleCollapse (NamedTagColumn c tth tl) =
    NamedTagColumn { c | collapsed = not c.collapsed } tth tl



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


isCompleted : String -> TaskItem -> Bool
isCompleted t taskItem =
    TaskItem.isCompleted taskItem
        || (TaskItem.hasSubtasks taskItem && TaskItem.allSubtasksWithMatchingTagCompleted t taskItem)
