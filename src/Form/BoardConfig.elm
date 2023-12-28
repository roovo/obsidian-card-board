module Form.BoardConfig exposing
    ( BoardConfigForm
    , fromNewBoardForm
    , init
    , mapColumnsForm
    , optionsForSelect
    , safeDecoder
    , toggleShowColumnTags
    , toggleShowFilteredTags
    , updateFilterPolarity
    , updateFilterScope
    , updateName
    )

import BoardConfig exposing (BoardConfig)
import Columns exposing (Columns)
import DefaultColumnNames exposing (DefaultColumnNames)
import Filter exposing (Filter)
import Form.Column as ColumnForm exposing (ColumnForm)
import Form.Columns as ColumnsForm exposing (ColumnsForm)
import Form.NewBoard as NewBoardForm exposing (NewBoardForm)
import Form.NewColumn as NewColumnForm exposing (NewColumnForm)
import Form.SafeDecoder as SD
import Form.Select exposing (Option)



-- TYPES


type alias BoardConfigForm =
    { columnsForm : ColumnsForm
    , filters : List Filter
    , filterPolarity : String
    , filterScope : String
    , name : String
    , showColumnTags : Bool
    , showFilteredTags : Bool
    }



-- CONSTRUCTION


fromNewBoardForm : DefaultColumnNames -> NewBoardForm -> BoardConfigForm
fromNewBoardForm defaultColumnNames newBoardConfigForm =
    let
        columnsForm =
            case newBoardConfigForm.boardType of
                "dateBoard" ->
                    ColumnsForm
                        [ ColumnForm.UndatedColumnForm False { name = DefaultColumnNames.nameFor "undated" defaultColumnNames }
                        , ColumnForm.DatedColumnForm False { name = DefaultColumnNames.nameFor "today" defaultColumnNames, rangeType = "Before", from = "", to = "1" }
                        , ColumnForm.DatedColumnForm False { name = DefaultColumnNames.nameFor "tomorrow" defaultColumnNames, rangeType = "Between", from = "1", to = "1" }
                        , ColumnForm.DatedColumnForm False { name = DefaultColumnNames.nameFor "future" defaultColumnNames, rangeType = "After", from = "1", to = "" }
                        , ColumnForm.CompletedColumnForm False { name = DefaultColumnNames.nameFor "completed" defaultColumnNames, limit = "10" }
                        ]

                "tagBoard" ->
                    ColumnsForm
                        [ ColumnForm.UntaggedColumnForm False { name = DefaultColumnNames.nameFor "untagged" defaultColumnNames }
                        , ColumnForm.OtherTagsColumnForm False { name = DefaultColumnNames.nameFor "otherTags" defaultColumnNames }
                        , ColumnForm.CompletedColumnForm False { name = DefaultColumnNames.nameFor "completed" defaultColumnNames, limit = "10" }
                        ]

                _ ->
                    ColumnsForm []
    in
    { columnsForm = columnsForm
    , filters = []
    , filterPolarity = "Allow"
    , filterScope = "Both"
    , name = newBoardConfigForm.name
    , showColumnTags = True
    , showFilteredTags = True
    }


init : BoardConfig -> BoardConfigForm
init boardConfig =
    let
        filterPolarity =
            case BoardConfig.filterPolarity boardConfig of
                Filter.Allow ->
                    "Allow"

                Filter.Deny ->
                    "Deny"

        filterScope =
            case BoardConfig.filterScope boardConfig of
                Filter.TopLevelOnly ->
                    "TopLevelOnly"

                Filter.SubTasksOnly ->
                    "SubTasksOnly"

                Filter.Both ->
                    "Both"
    in
    { columnsForm = ColumnsForm.init <| BoardConfig.columns boardConfig
    , filters = BoardConfig.filters boardConfig
    , filterPolarity = filterPolarity
    , filterScope = filterScope
    , name = BoardConfig.name boardConfig
    , showColumnTags = BoardConfig.showColumnTags boardConfig
    , showFilteredTags = BoardConfig.showFilteredTags boardConfig
    }


safeDecoder : SD.Decoder BoardConfigForm BoardConfig
safeDecoder =
    SD.map7 BoardConfig.Config
        columnsDecoder
        filtersDecoder
        filterPolarityDecoder
        filterScopeDecoder
        nameDecoder
        showColumnTagsDecoder
        showFilteredTagsDecoder
        |> SD.map BoardConfig.fromConfig



-- INFO


optionsForSelect : NewColumnForm -> Maybe BoardConfigForm -> List Option
optionsForSelect newColumnForm boardConfigForm =
    case boardConfigForm of
        Just configForm ->
            ColumnsForm.optionsForSelect configForm.columnsForm newColumnForm

        Nothing ->
            []



-- MODIFICATION


mapColumnsForm : (ColumnsForm -> ColumnsForm) -> BoardConfigForm -> BoardConfigForm
mapColumnsForm fn boardConfigForm =
    { boardConfigForm | columnsForm = fn boardConfigForm.columnsForm }


toggleShowColumnTags : BoardConfigForm -> BoardConfigForm
toggleShowColumnTags boardConfigForm =
    { boardConfigForm | showColumnTags = not boardConfigForm.showColumnTags }


toggleShowFilteredTags : BoardConfigForm -> BoardConfigForm
toggleShowFilteredTags boardConfigForm =
    { boardConfigForm | showFilteredTags = not boardConfigForm.showFilteredTags }


updateFilterPolarity : String -> BoardConfigForm -> BoardConfigForm
updateFilterPolarity newPolarity boardConfigForm =
    { boardConfigForm | filterPolarity = newPolarity }


updateFilterScope : String -> BoardConfigForm -> BoardConfigForm
updateFilterScope newScope boardConfigForm =
    { boardConfigForm | filterScope = newScope }


updateName : String -> BoardConfigForm -> BoardConfigForm
updateName newName boardConfigForm =
    { boardConfigForm | name = newName }



-- PRIVATE


columnsDecoder : SD.Decoder BoardConfigForm Columns
columnsDecoder =
    ColumnsForm.safeDecoder
        |> SD.lift .columnsForm


filtersDecoder : SD.Decoder BoardConfigForm (List Filter)
filtersDecoder =
    SD.identity
        |> SD.lift .filters


filterPolarityDecoder : SD.Decoder BoardConfigForm Filter.Polarity
filterPolarityDecoder =
    (SD.custom <|
        \polarity ->
            case polarity of
                "Deny" ->
                    Ok Filter.Deny

                _ ->
                    Ok Filter.Allow
    )
        |> SD.lift .filterPolarity


filterScopeDecoder : SD.Decoder BoardConfigForm Filter.Scope
filterScopeDecoder =
    (SD.custom <|
        \scope ->
            case scope of
                "TopLevelOnly" ->
                    Ok Filter.TopLevelOnly

                "SubTasksOnly" ->
                    Ok Filter.SubTasksOnly

                _ ->
                    Ok Filter.Both
    )
        |> SD.lift .filterScope


nameDecoder : SD.Decoder BoardConfigForm String
nameDecoder =
    SD.identity
        |> SD.lift String.trim
        |> SD.lift .name


showColumnTagsDecoder : SD.Decoder BoardConfigForm Bool
showColumnTagsDecoder =
    SD.identity
        |> SD.lift .showColumnTags


showFilteredTagsDecoder : SD.Decoder BoardConfigForm Bool
showFilteredTagsDecoder =
    SD.identity
        |> SD.lift .showFilteredTags
