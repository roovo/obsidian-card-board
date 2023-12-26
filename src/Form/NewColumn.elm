module Form.NewColumn exposing
    ( NewColumnForm
    , default
    , updateColumnType
    , updateName
    )

import Form.Column as ColumnForm
import Form.SafeDecoder as SD



-- TYPES


type alias NewColumnForm =
    { name : String
    , columnType : String
    }



-- fromColumn : NewColumnForm -> Maybe Form
-- fromColumn newColumnForm =
--     case newColumnForm.columnType of
--         "Completed" ->
--             Just (CompletedColumnForm { name = newColumnForm.name, limit = "10" })
--
--         "Dated" ->
--             Just (DatedColumnForm { name = newColumnForm.name, rangeType = "Before", from = "", to = "" })
--
--         "NamedTag" ->
--             Just (NamedTagColumnForm { name = newColumnForm.name, tag = "" })
--
--         "OtherTags" ->
--             Just (OtherTagsColumnForm { name = newColumnForm.name })
--
--         "Undated" ->
--             Just (UndatedColumnForm { name = newColumnForm.name })
--
--         "Untagged" ->
--             Just (UntaggedColumnForm { name = newColumnForm.name })
--
--         _ ->
--             Nothing
--
-- INITIALIZE


default : NewColumnForm
default =
    { name = ""
    , columnType = ""
    }



-- MODIFICATION


updateColumnType : String -> NewColumnForm -> NewColumnForm
updateColumnType newType newColumnForm =
    { newColumnForm | columnType = newType }


updateName : String -> NewColumnForm -> NewColumnForm
updateName newName newColumnForm =
    { newColumnForm | name = newName }
