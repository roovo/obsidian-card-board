module Task exposing (Todo)

-- TYPES


type Todo
    = Todo TodoData


type alias TodoData =
    { file : String
    , title : String
    }



-- SERIALIZATION


taskDecoder : Decoder TodoData
taskDecoder =
    Decode.succeed TodoData
