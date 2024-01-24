module Columns exposing
    ( Columns
    , addTaskItem
    , addTaskList
    , cleanupNames
    , collapseColumn
    , decoder
    , empty
    , encoder
    , fromList
    , namedTagColumnTags
    , restrictSpecialColumns
    , setNamesToDefault
    , toList
    , updateOtherTags
    )

import Column exposing (Column)
import Column.Completed as CompletedColumn exposing (CompletedColumn)
import Column.OtherTags exposing (OtherTagsColumn)
import Date exposing (Date)
import DefaultColumnNames exposing (DefaultColumnNames)
import List.Extra as LE
import Maybe.Extra as ME
import PlacementResult exposing (PlacementResult)
import Set exposing (Set)
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode



-- TYPES


type Columns
    = WithCompleted (List Column) CompletedColumn
    | WithoutCompleted (List Column)



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


namedTagColumnTags : Columns -> List String
namedTagColumnTags columns =
    columns
        |> toList
        |> List.map Column.namedTagTag
        |> ME.values



-- MODIFICATION


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


addTaskList : Date -> List String -> Columns -> TaskList -> Columns
addTaskList today tagsToHide columns taskList =
    taskList
        |> TaskList.foldl (addTaskItem today) columns
        |> setTagsToHide tagsToHide


cleanupNames : DefaultColumnNames -> Columns -> Columns
cleanupNames defaultColumnNames columns =
    let
        defaultNameIfBlank : Column -> Column
        defaultNameIfBlank column =
            if String.isEmpty (Column.name column) then
                Column.setNameToDefault defaultColumnNames column

            else
                column

        fillBlankName : Column -> Column
        fillBlankName column =
            if String.isEmpty (Column.name column) then
                Column.updateName "Unnamed" column

            else
                column

        trimName : Column -> Column
        trimName column =
            Column.updateName (String.trim <| Column.name column) column

        uniqueNameHelper : Int -> Column -> ( List String, List Column ) -> ( List String, List Column )
        uniqueNameHelper index column ( namesAcc, columnsAcc ) =
            let
                nextNamesAcc : List String
                nextNamesAcc =
                    String.replace " " "_" uniqueName :: namesAcc

                renamedColumn : Column
                renamedColumn =
                    Column.updateName uniqueName column

                uniqueName : String
                uniqueName =
                    if List.member (String.replace " " "_" <| Column.name column) namesAcc then
                        Column.name column ++ "." ++ String.fromInt index

                    else
                        Column.name column
            in
            ( nextNamesAcc, List.append columnsAcc [ renamedColumn ] )
    in
    columns
        |> toList
        |> List.map trimName
        |> List.map defaultNameIfBlank
        |> List.map fillBlankName
        |> LE.indexedFoldl uniqueNameHelper ( [], [] )
        |> Tuple.second
        |> fromList


collapseColumn : Int -> Bool -> Columns -> Columns
collapseColumn columnIndex isCollapsed columns =
    columns
        |> toList
        |> LE.updateAt columnIndex (Column.setCollapse isCollapsed)
        |> fromList


restrictSpecialColumns : Columns -> Columns
restrictSpecialColumns columns =
    let
        restrictHelper : Column -> ( Set String, List Column ) -> ( Set String, List Column )
        restrictHelper column ( typesAcc, columnsAcc ) =
            let
                specialTypes : Set String
                specialTypes =
                    Set.fromList [ "Other Tags", "Undated", "Untagged" ]

                nextTypesAcc : Set String
                nextTypesAcc =
                    Set.insert thisTypeString typesAcc

                thisTypeString : String
                thisTypeString =
                    Column.typeString column

                nextColumnsAcc : List Column
                nextColumnsAcc =
                    if
                        Set.member thisTypeString typesAcc
                            && Set.member thisTypeString specialTypes
                    then
                        columnsAcc

                    else
                        column :: columnsAcc
            in
            ( nextTypesAcc, nextColumnsAcc )
    in
    columns
        |> toList
        |> List.foldl restrictHelper ( Set.empty, [] )
        |> Tuple.second
        |> List.reverse
        |> fromList


setNamesToDefault : DefaultColumnNames -> Columns -> Columns
setNamesToDefault defaultColumnNames columns =
    columns
        |> toList
        |> List.map (Column.setNameToDefault defaultColumnNames)
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
