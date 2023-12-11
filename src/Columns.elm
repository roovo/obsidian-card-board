module Columns exposing
    ( Columns
    , OptionsForSelect
    , addColumn
    , addTaskList
    , collapseColumn
    , completedLimit
    , decoder
    , empty
    , encoder
    , fromList
    , namedTagColumnTags
    , optionsForSelect
    , setNamesToDefault
    , toList
    , updateColumnName
    , updateCompletedColumnLimit
    , updateDatedColumnRangeType
    , updateDatedColumnRangeValueFrom
    , updateDatedColumnRangeValueTo
    , updateOtherTags
    )

import Column exposing (Column)
import Column.Completed as CompletedColumn exposing (CompletedColumn)
import Column.OtherTags exposing (OtherTagsColumn)
import Date exposing (Date)
import DefaultColumnNames exposing (DefaultColumnNames)
import List.Extra as LE
import Maybe.Extra as ME
import NewColumnConfig exposing (NewColumnConfig)
import PlacementResult exposing (PlacementResult)
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode



-- TYPES


type Columns
    = WithCompleted (List Column) CompletedColumn
    | WithoutCompleted (List Column)


type alias OptionsForSelect =
    { isSelected : Bool
    , text : String
    , value : String
    }



-- CONSTRUCTION


empty : Columns
empty =
    WithoutCompleted []


fromList : List Column -> Columns
fromList columns =
    let
        completed : Maybe CompletedColumn
        completed =
            LE.find Column.isCompleted columns
                |> Maybe.map Column.asCompletedColumn
                |> ME.join

        completedIndex : Maybe Int
        completedIndex =
            LE.findIndex Column.isCompleted columns

        notCompleted : List Column
        notCompleted =
            LE.filterNot Column.isCompleted columns
    in
    case ( completed, completedIndex ) of
        ( Just completedColumn, Just index ) ->
            WithCompleted notCompleted (CompletedColumn.setIndex index completedColumn)

        _ ->
            WithoutCompleted notCompleted



-- DECODE / ENCODE


decoder : TsDecode.Decoder Columns
decoder =
    TsDecode.list Column.decoder
        |> TsDecode.map fromList


encoder : TsEncode.Encoder Columns
encoder =
    TsEncode.map toList <| TsEncode.list Column.encoder



-- INFO


completedLimit : Columns -> Int
completedLimit columns =
    case columns of
        WithCompleted _ completedColumn ->
            CompletedColumn.limit completedColumn

        WithoutCompleted _ ->
            0


namedTagColumnTags : Columns -> List String
namedTagColumnTags columns =
    columns
        |> toList
        |> List.map Column.namedTagTag
        |> ME.values


optionsForSelect : Columns -> NewColumnConfig -> List OptionsForSelect
optionsForSelect columns newColumnConfig =
    let
        alreadyHasCompleted : Bool
        alreadyHasCompleted =
            columns
                |> toList
                |> List.any Column.isCompleted

        alreadyHasOtherTags : Bool
        alreadyHasOtherTags =
            columns
                |> toList
                |> List.any Column.isOtherTags

        alreadyHasUndated : Bool
        alreadyHasUndated =
            columns
                |> toList
                |> List.any Column.isUndated

        alreadyHasUntagged : Bool
        alreadyHasUntagged =
            columns
                |> toList
                |> List.any Column.isUntagged

        completed : List OptionsForSelect
        completed =
            if alreadyHasCompleted then
                []

            else
                [ { isSelected = newColumnConfig.columnType == "completed"
                  , text = "Completed"
                  , value = "completed"
                  }
                ]

        otherTags : List OptionsForSelect
        otherTags =
            if alreadyHasOtherTags then
                []

            else
                [ { isSelected = newColumnConfig.columnType == "otherTags"
                  , text = "Other Tags"
                  , value = "otherTags"
                  }
                ]

        undated : List OptionsForSelect
        undated =
            if alreadyHasUndated then
                []

            else
                [ { isSelected = newColumnConfig.columnType == "undated"
                  , text = "Undated"
                  , value = "undated"
                  }
                ]

        untagged : List OptionsForSelect
        untagged =
            if alreadyHasUntagged then
                []

            else
                [ { isSelected = newColumnConfig.columnType == "untagged"
                  , text = "Untagged"
                  , value = "untagged"
                  }
                ]

        allColumns =
            completed
                ++ [ { isSelected = newColumnConfig.columnType == "dated"
                     , text = "Dated"
                     , value = "dated"
                     }
                   ]
                ++ otherTags
                ++ [ { isSelected = newColumnConfig.columnType == "namedTag"
                     , text = "Tagged"
                     , value = "namedTag"
                     }
                   ]
                ++ undated
                ++ untagged
    in
    if List.any .isSelected allColumns then
        allColumns

    else
        allColumns
            |> LE.updateAt 0 (\ofs -> { ofs | isSelected = True })



-- MODIFICATION


addColumn : DefaultColumnNames -> NewColumnConfig -> Columns -> Columns
addColumn defaultColumnNames newColumnConfig columns =
    let
        allColumns : List Column
        allColumns =
            toList columns

        completedPosition : Maybe Int
        completedPosition =
            LE.findIndex Column.isCompleted allColumns

        newColumn : List Column
        newColumn =
            Column.fromColumnConfig defaultColumnNames newColumnConfig
                |> Maybe.map List.singleton
                |> Maybe.withDefault []
    in
    (case completedPosition of
        Just position ->
            let
                ( preCompleted, completedPlus ) =
                    LE.splitAt position allColumns
            in
            if List.length completedPlus == 1 then
                preCompleted ++ newColumn ++ completedPlus

            else
                allColumns ++ newColumn

        Nothing ->
            allColumns ++ newColumn
    )
        |> fromList


addTaskList : Date -> List String -> Columns -> TaskList -> Columns
addTaskList today tagsToHide columns taskList =
    taskList
        |> TaskList.foldl (addTaskItem today) columns
        |> setTagsToHide tagsToHide


collapseColumn : Int -> Bool -> Columns -> Columns
collapseColumn columnIndex isCollapsed columns =
    columns
        |> toList
        |> LE.updateAt columnIndex (Column.setCollapse isCollapsed)
        |> fromList


setNamesToDefault : DefaultColumnNames -> Columns -> Columns
setNamesToDefault defaultColumnNames columns =
    columns
        |> toList
        |> List.map (Column.setNameToDefault defaultColumnNames)
        |> fromList


updateColumnName : Int -> String -> Columns -> Columns
updateColumnName index newName columns =
    columns
        |> toList
        |> LE.updateAt index (Column.updateName newName)
        |> fromList


updateCompletedColumnLimit : Int -> Int -> Columns -> Columns
updateCompletedColumnLimit index newLimit columns =
    columns
        |> toList
        |> LE.updateAt index (Column.updateCompletedColumnLimit newLimit)
        |> fromList


updateDatedColumnRangeType : Int -> String -> Columns -> Columns
updateDatedColumnRangeType index rangeType columns =
    columns
        |> toList
        |> LE.updateAt index (Column.updateDatedColumnRangeType rangeType)
        |> fromList


updateDatedColumnRangeValueFrom : Int -> Int -> Columns -> Columns
updateDatedColumnRangeValueFrom index newValue columns =
    columns
        |> toList
        |> LE.updateAt index (Column.updateDatedColumnRangeValueFrom newValue)
        |> fromList


updateDatedColumnRangeValueTo : Int -> Int -> Columns -> Columns
updateDatedColumnRangeValueTo index newValue columns =
    columns
        |> toList
        |> LE.updateAt index (Column.updateDatedColumnRangeValueTo newValue)
        |> fromList


updateOtherTags : (OtherTagsColumn -> OtherTagsColumn) -> Columns -> Columns
updateOtherTags fn columns =
    columns
        |> toList
        |> List.map (Column.updateOtherTags fn)
        |> fromList



-- CONVERSION


toList : Columns -> List Column
toList columns =
    case columns of
        WithCompleted nonCompletedConfigs completedConfig ->
            nonCompletedConfigs
                |> insert (CompletedColumn.index completedConfig) (Column.completed completedConfig)

        WithoutCompleted nonCompletedConfigs ->
            nonCompletedConfigs



-- PRIVATE


addTaskItem : Date -> TaskItem -> Columns -> Columns
addTaskItem today taskItem columns =
    case columns of
        WithCompleted nonCompletedConfigs completedConfig ->
            let
                ( newConfigs, allPlacementResults ) =
                    addWithPlacement today taskItem nonCompletedConfigs
            in
            WithCompleted newConfigs
                (CompletedColumn.addTaskItem allPlacementResults taskItem completedConfig)

        WithoutCompleted nonCompletedConfigs ->
            let
                ( newConfigs, _ ) =
                    addWithPlacement today taskItem nonCompletedConfigs
            in
            WithoutCompleted newConfigs


addWithPlacement : Date -> TaskItem -> List Column -> ( List Column, List PlacementResult )
addWithPlacement today taskItem initialConfigs =
    let
        fn : Column -> ( List Column, List PlacementResult ) -> ( List Column, List PlacementResult )
        fn thisConfig ( accumulatedConfigs, placementResults ) =
            Column.addTaskItem today taskItem thisConfig
                |> Tuple.mapFirst (\c -> c :: accumulatedConfigs)
                |> Tuple.mapSecond (\r -> r :: placementResults)
    in
    List.foldr fn ( [], [] ) initialConfigs


insert : Int -> a -> List a -> List a
insert i value list =
    List.take i list ++ [ value ] ++ List.drop i list


setTagsToHide : List String -> Columns -> Columns
setTagsToHide tags columns =
    case columns of
        WithCompleted nonCompletedConfigs completedConfig ->
            WithCompleted (List.map (Column.setTagsToHide tags) nonCompletedConfigs) (CompletedColumn.setTagsToHide tags completedConfig)

        WithoutCompleted nonCompletedConfigs ->
            WithoutCompleted (List.map (Column.setTagsToHide tags) nonCompletedConfigs)
