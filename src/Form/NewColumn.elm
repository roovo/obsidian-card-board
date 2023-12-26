module Form.NewColumn exposing
    ( NewColumnForm
    , default
    , safeDecoder
    , updateColumnType
    , updateName
    )

import Form.Column as ColumnForm exposing (ColumnForm)
import Form.SafeDecoder as SD



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
                    Ok <| Just (ColumnForm.CompletedColumnForm { name = form.name, limit = "10" })

                "Dated" ->
                    Ok <| Just (ColumnForm.DatedColumnForm { name = form.name, rangeType = "Before", from = "", to = "" })

                "NamedTag" ->
                    Ok <| Just (ColumnForm.NamedTagColumnForm { name = form.name, tag = "" })

                "OtherTags" ->
                    Ok <| Just (ColumnForm.OtherTagsColumnForm { name = form.name })

                "Undated" ->
                    Ok <| Just (ColumnForm.UndatedColumnForm { name = form.name })

                "Untagged" ->
                    Ok <| Just (ColumnForm.UntaggedColumnForm { name = form.name })

                _ ->
                    Ok <| Nothing



-- MODIFICATION


updateColumnType : String -> NewColumnForm -> NewColumnForm
updateColumnType newType newColumnForm =
    { newColumnForm | columnType = newType }


updateName : String -> NewColumnForm -> NewColumnForm
updateName newName newColumnForm =
    { newColumnForm | name = newName }
