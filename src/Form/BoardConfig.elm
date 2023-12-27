module Form.BoardConfig exposing
    ( BoardConfigForm
    , init
    , mapColumnsForm
    , safeDecoder
    , toggleShowColumnTags
    , toggleShowFilteredTags
    , updateFilterPolarity
    , updateFilterScope
    , updateName
    )

import BoardConfig exposing (BoardConfig)
import Columns exposing (Columns)
import DefaultColumnNames
import Filter exposing (Filter)
import Form.Columns as ColumnsForm exposing (ColumnsForm)
import Form.NewBoard as NewBoardForm
import Form.SafeDecoder as SD



-- TYPES


type alias BoardConfigForm =
    { columns : ColumnsForm
    , filters : List Filter
    , filterPolarity : String
    , filterScope : String
    , name : String
    , showColumnTags : Bool
    , showFilteredTags : Bool
    }



-- CONSTRUCTION


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
    { columns = ColumnsForm.init <| BoardConfig.columns boardConfig
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



-- MODIFICATION


mapColumnsForm : (ColumnsForm -> ColumnsForm) -> BoardConfigForm -> BoardConfigForm
mapColumnsForm fn boardConfigForm =
    { boardConfigForm | columns = fn boardConfigForm.columns }


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
        |> SD.lift .columns


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
