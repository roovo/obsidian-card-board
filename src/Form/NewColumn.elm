module Form.NewColumn exposing
    ( NewColumnForm
    , default
    , optionsForSelect
    , safeDecoder
    , updateColumnType
    , updateName
    )

import Form.Column as ColumnForm exposing (ColumnForm)
import Form.Columns exposing (ColumnsForm)
import Form.SafeDecoder as SD
import Form.Select exposing (Option)
import List.Extra as LE



-- TYPES


type alias NewColumnForm =
    { name : String
    , columnType : String
    }



-- INITIALIZE


default : NewColumnForm
default =
    { name = ""
    , columnType = ""
    }



-- DECODE


safeDecoder : SD.Decoder NewColumnForm (Maybe ColumnForm)
safeDecoder =
    SD.custom <|
        \form ->
            case form.columnType of
                "Completed" ->
                    Ok <| Just (ColumnForm.CompletedColumnForm False { name = form.name, limit = "10" })

                "Dated" ->
                    Ok <| Just (ColumnForm.DatedColumnForm False { name = form.name, rangeType = "Before", from = "", to = "" })

                "NamedTag" ->
                    Ok <| Just (ColumnForm.NamedTagColumnForm False { name = form.name, tag = "" })

                "OtherTags" ->
                    Ok <| Just (ColumnForm.OtherTagsColumnForm False { name = form.name })

                "Undated" ->
                    Ok <| Just (ColumnForm.UndatedColumnForm False { name = form.name })

                "Untagged" ->
                    Ok <| Just (ColumnForm.UntaggedColumnForm False { name = form.name })

                _ ->
                    Ok <| Nothing



-- INFO


optionsForSelect : ColumnsForm -> NewColumnForm -> List Option
optionsForSelect form newColumnConfigForm =
    let
        alreadyHasCompleted : Bool
        alreadyHasCompleted =
            form
                |> .columnForms
                |> List.any ColumnForm.isCompleted

        alreadyHasOtherTags : Bool
        alreadyHasOtherTags =
            form
                |> .columnForms
                |> List.any ColumnForm.isOtherTags

        alreadyHasUndated : Bool
        alreadyHasUndated =
            form
                |> .columnForms
                |> List.any ColumnForm.isUndated

        alreadyHasUntagged : Bool
        alreadyHasUntagged =
            form
                |> .columnForms
                |> List.any ColumnForm.isUntagged

        completed : List Option
        completed =
            if alreadyHasCompleted then
                []

            else
                [ { isSelected = newColumnConfigForm.columnType == "completed"
                  , text = "Completed"
                  , value = "Completed"
                  }
                ]

        otherTags : List Option
        otherTags =
            if alreadyHasOtherTags then
                []

            else
                [ { isSelected = newColumnConfigForm.columnType == "otherTags"
                  , text = "Other Tags"
                  , value = "OtherTags"
                  }
                ]

        undated : List Option
        undated =
            if alreadyHasUndated then
                []

            else
                [ { isSelected = newColumnConfigForm.columnType == "undated"
                  , text = "Undated"
                  , value = "Undated"
                  }
                ]

        untagged : List Option
        untagged =
            if alreadyHasUntagged then
                []

            else
                [ { isSelected = newColumnConfigForm.columnType == "untagged"
                  , text = "Untagged"
                  , value = "Untagged"
                  }
                ]

        allColumns : List Option
        allColumns =
            completed
                ++ [ { isSelected = newColumnConfigForm.columnType == "dated"
                     , text = "Dated"
                     , value = "Dated"
                     }
                   ]
                ++ otherTags
                ++ [ { isSelected = newColumnConfigForm.columnType == "namedTag"
                     , text = "Tagged"
                     , value = "NamedTag"
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


updateColumnType : String -> NewColumnForm -> NewColumnForm
updateColumnType newType newColumnForm =
    { newColumnForm | columnType = newType }


updateName : String -> NewColumnForm -> NewColumnForm
updateName newName newColumnForm =
    { newColumnForm | name = newName }
