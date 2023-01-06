module TagBoardColumns exposing
    ( TagBoardColumns
    , addTaskItem
    , columns
    , init
    )

import Column exposing (Column)
import Column.NamedTag as NamedTagColumn exposing (NamedTagColumn)
import Column.OtherTags as OtherTagsColumn exposing (OtherTagsColumn)
import Column.Untagged as UntaggedColumn exposing (UntaggedColumn)
import ColumnNames exposing (ColumnNames)
import Filter exposing (Filter, Polarity)
import List.Extra as LE
import Parser as P exposing ((|.), (|=), Parser)
import ParserHelper
import String.Extra as SE
import TagBoard
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
    }



-- BUILDING


init : ColumnNames -> TagBoard.Config -> TagBoardColumns
init columnNames tagBoardConfig =
    TagBoardColumns
        { untaggedColumn = UntaggedColumn.init tagBoardConfig columnNames
        , otherTagsColumn = OtherTagsColumn.init tagBoardConfig columnNames
        , namedTagColumns =
            List.map
                (NamedTagColumn.init tagBoardConfig)
                (LE.uniqueBy .tag tagBoardConfig.columns)
        }


addTaskItem : TaskItem -> TagBoardColumns -> TagBoardColumns
addTaskItem taskItem (TagBoardColumns config) =
    let
        addToNamed : NamedTagColumn -> NamedTagColumn
        addToNamed namedTagColumn =
            Tuple.first (NamedTagColumn.addTaskItem taskItem namedTagColumn)
    in
    TagBoardColumns
        { config
            | untaggedColumn = Tuple.first (UntaggedColumn.addTaskItem taskItem config.untaggedColumn)
            , otherTagsColumn = Tuple.first (OtherTagsColumn.addTaskItem taskItem config.otherTagsColumn)
            , namedTagColumns = List.map addToNamed config.namedTagColumns
        }



-- INFO


columns : TagBoardColumns -> List (Column TaskItem)
columns (TagBoardColumns config) =
    List.map NamedTagColumn.asColumn config.namedTagColumns
        |> List.append [ OtherTagsColumn.asColumn config.otherTagsColumn ]
        |> List.append [ UntaggedColumn.asColumn config.untaggedColumn ]
        |> List.filter Column.isEnabled
