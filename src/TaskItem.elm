module TaskItem exposing (TaskItem)

-- TYPES


type TaskItem
    = TaskItem TaskItemData


type alias TaskItemData =
    { file : String
    , title : String
    }



-- SERIALIZATION


taskDecoder : Decoder TaskItemData
taskDecoder =
    Decode.succeed TaskItemData
