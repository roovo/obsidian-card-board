module NewColumnConfig exposing
    ( NewColumnConfig
    , OptionsForSelect
    , default
    , optionsForSelect
    , updateColumnType
    , updateName
    )

import Column
import Columns exposing (Columns)
import List.Extra as LE



-- TYPES


type alias NewColumnConfig =
    { name : String
    , columnType : String
    }


type alias OptionsForSelect =
    { isSelected : Bool
    , text : String
    , value : String
    }



-- INITIALIZE


default : NewColumnConfig
default =
    { name = ""
    , columnType = ""
    }



-- INFO


optionsForSelect : Columns -> NewColumnConfig -> List OptionsForSelect
optionsForSelect columns newColumnConfig =
    let
        alreadyHasCompleted : Bool
        alreadyHasCompleted =
            columns
                |> Columns.toList
                |> List.any Column.isCompleted

        alreadyHasOtherTags : Bool
        alreadyHasOtherTags =
            columns
                |> Columns.toList
                |> List.any Column.isOtherTags

        alreadyHasUndated : Bool
        alreadyHasUndated =
            columns
                |> Columns.toList
                |> List.any Column.isUndated

        alreadyHasUntagged : Bool
        alreadyHasUntagged =
            columns
                |> Columns.toList
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


updateColumnType : String -> NewColumnConfig -> NewColumnConfig
updateColumnType newType newColumnConfig =
    { newColumnConfig | columnType = newType }


updateName : String -> NewColumnConfig -> NewColumnConfig
updateName newName newColumnConfig =
    { newColumnConfig | name = newName }
