module Column.Undated exposing
    ( FormError(..)
    , UndatedColumn
    , addTaskItem
    , decoder
    , encoder
    , formDecoder
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
import Form.Decoder as FD
import PlacementResult exposing (PlacementResult)
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)
import TsJson.Decode as TsDecode
import TsJson.Decode.Pipeline as TsDecode
import TsJson.Encode as TsEncode



-- TYPES


type UndatedColumn
    = UndatedColumn Config (List String) TaskList


type alias Config =
    { collapsed : Bool
    , name : String
    }


type alias Form =
    { name : String
    }


type FormError
    = NameRequired



-- CONSTRUCTION


init : String -> UndatedColumn
init name_ =
    UndatedColumn { collapsed = False, name = name_ } [] TaskList.empty



-- DECODE / ENCODE


decoder : TsDecode.Decoder UndatedColumn
decoder =
    (TsDecode.succeed Config
        |> TsDecode.required "collapsed" TsDecode.bool
        |> TsDecode.required "name" TsDecode.string
    )
        |> TsDecode.map (\c -> UndatedColumn c [] TaskList.empty)


encoder : TsEncode.Encoder UndatedColumn
encoder =
    TsEncode.map config configEncoder


formDecoder : FD.Decoder Form FormError UndatedColumn
formDecoder =
    FD.map init formNameDecoder



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


setCollapse : Bool -> UndatedColumn -> UndatedColumn
setCollapse isCollapsed_ (UndatedColumn c tth tl) =
    UndatedColumn { c | collapsed = isCollapsed_ } tth tl


setNameToDefault : DefaultColumnNames -> UndatedColumn -> UndatedColumn
setNameToDefault defaultColumnNames (UndatedColumn c tth tl) =
    UndatedColumn { c | name = DefaultColumnNames.nameFor "undated" defaultColumnNames } tth tl


setTagsToHide : List String -> UndatedColumn -> UndatedColumn
setTagsToHide tags (UndatedColumn c _ tl) =
    UndatedColumn c tags tl


toggleCollapse : UndatedColumn -> UndatedColumn
toggleCollapse (UndatedColumn c tth tl) =
    UndatedColumn { c | collapsed = not c.collapsed } tth tl


updateName : String -> UndatedColumn -> UndatedColumn
updateName newName (UndatedColumn c tth tl) =
    UndatedColumn { c | name = newName } tth tl



-- PRIVATE


config : UndatedColumn -> Config
config (UndatedColumn c _ _) =
    c


configEncoder : TsEncode.Encoder Config
configEncoder =
    TsEncode.object
        [ TsEncode.required "collapsed" .collapsed TsEncode.bool
        , TsEncode.required "name" .name TsEncode.string
        ]


formNameDecoder : FD.Decoder Form FormError String
formNameDecoder =
    FD.identity
        |> required NameRequired
        |> FD.lift .name


required : err -> FD.Decoder String err a -> FD.Decoder String err a
required error d =
    FD.with <|
        \a ->
            case a of
                "" ->
                    FD.fail error

                _ ->
                    FD.lift identity d
