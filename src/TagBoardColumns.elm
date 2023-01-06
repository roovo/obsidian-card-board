module TagBoardColumns exposing
    ( TagBoardColumns
    , addTaskItem
    , columns
    , init
    )

import Column exposing (Column, PlacementResult)
import Column.Completed as CompletedColumn exposing (CompletedColumn)
import Column.NamedTag as NamedTagColumn exposing (NamedTagColumn)
import Column.OtherTags as OtherTagsColumn exposing (OtherTagsColumn)
import Column.Untagged as UntaggedColumn exposing (UntaggedColumn)
import ColumnNames exposing (ColumnNames)
import Filter exposing (Filter, Polarity)
import List.Extra as LE
import Parser as P exposing ((|.), (|=), Parser)
import ParserHelper
import String.Extra as SE
import TagBoardConfig exposing (TagBoardConfig)
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode



-- TYPES


type TagBoardColumns
    = TagBoardColumns Config


type alias Config =
    { untaggedColumn : UntaggedColumn
    , otherTagsColumn : OtherTagsColumn
    , namedTagColumns : List NamedTagColumn
    , completedColumn : CompletedColumn
    }



-- BUILDING


init : ColumnNames -> TagBoardConfig -> TagBoardColumns
init columnNames tagBoardConfig =
    TagBoardColumns
        { untaggedColumn = UntaggedColumn.init tagBoardConfig columnNames
        , otherTagsColumn = OtherTagsColumn.init tagBoardConfig columnNames
        , namedTagColumns =
            List.map
                (NamedTagColumn.init tagBoardConfig)
                (LE.uniqueBy .tag tagBoardConfig.columns)
        , completedColumn = CompletedColumn.init tagBoardConfig columnNames
        }


addTaskItem : TaskItem -> TagBoardColumns -> TagBoardColumns
addTaskItem taskItem (TagBoardColumns config) =
    let
        addToOtherTagsColumn : ( Config, List PlacementResult )
        addToOtherTagsColumn =
            let
                ( newColumn, pr ) =
                    OtherTagsColumn.addTaskItem taskItem config.otherTagsColumn
            in
            ( { config | otherTagsColumn = newColumn }, [ pr ] )

        addToUntaggedColumn : ( Config, List PlacementResult ) -> ( Config, List PlacementResult )
        addToUntaggedColumn ( c, prs ) =
            let
                ( newColumn, pr ) =
                    UntaggedColumn.addTaskItem taskItem config.untaggedColumn
            in
            ( { c | untaggedColumn = newColumn }, pr :: prs )

        addToNamedTagColumn : NamedTagColumn -> ( NamedTagColumn, PlacementResult )
        addToNamedTagColumn namedTagColumn =
            NamedTagColumn.addTaskItem taskItem namedTagColumn

        addToNamedTagColumns : ( Config, List PlacementResult ) -> ( Config, List PlacementResult )
        addToNamedTagColumns ( c, prs ) =
            let
                ( newColumns, pr ) =
                    List.foldl bar ( [], [] ) config.namedTagColumns

                bar : NamedTagColumn -> ( List NamedTagColumn, List PlacementResult ) -> ( List NamedTagColumn, List PlacementResult )
                bar ntc ( ntcs, pr_ ) =
                    addToNamedTagColumn ntc
                        |> Tuple.mapFirst (\r -> r :: ntcs)
                        |> Tuple.mapSecond (\r -> r :: pr_)
            in
            ( { c | namedTagColumns = newColumns }, pr ++ prs )

        addToCompletedColumn : ( Config, List PlacementResult ) -> Config
        addToCompletedColumn ( c, prs ) =
            { c | completedColumn = CompletedColumn.addTaskItem prs taskItem config.completedColumn }
    in
    addToOtherTagsColumn
        |> addToUntaggedColumn
        |> addToNamedTagColumns
        |> addToCompletedColumn
        |> TagBoardColumns



-- INFO


columns : TagBoardColumns -> List (Column TaskItem)
columns (TagBoardColumns config) =
    [ CompletedColumn.asColumn config.completedColumn ]
        |> List.append (List.map NamedTagColumn.asColumn config.namedTagColumns)
        |> List.append [ OtherTagsColumn.asColumn config.otherTagsColumn ]
        |> List.append [ UntaggedColumn.asColumn config.untaggedColumn ]
        |> List.filter Column.isEnabled
