module Form.BoardConfig exposing
    ( Form
    , safeDecoder
    )

import BoardConfig exposing (BoardConfig)
import DefaultColumnNames
import Form.NewBoardConfig as NewBoardConfigForm
import Form.SafeDecoder as SD



-- TYPES
-- type FO
--     = BoardConfig Config


type alias Form =
    -- { columns : Columns
    -- , filters : List Filter
    -- , filterPolarity : String
    -- , filterScope : String
    { name : String
    , showColumnTags : Bool
    , showFilteredTags : Bool
    }


safeDecoder : SD.Decoder Form BoardConfig
safeDecoder =
    -- (TsDecode.succeed Config
    --     |> TsDecode.required "columns" Columns.decoder
    --     |> TsDecode.required "filters" (TsDecode.list Filter.decoder)
    --     |> TsDecode.required "filterPolarity" Filter.polarityDecoder
    --     |> TsDecode.required "filterScope" Filter.scopeDecoder
    --     |> TsDecode.required "name" TsDecode.string
    --     |> TsDecode.required "showColumnTags" TsDecode.bool
    --     |> TsDecode.required "showFilteredTags" TsDecode.bool
    -- )
    --     |> TsDecode.map configureOtherTagsColumn
    --     |> TsDecode.map BoardConfig
    SD.always <| BoardConfig.fromNewBoardConfig DefaultColumnNames.default NewBoardConfigForm.default
