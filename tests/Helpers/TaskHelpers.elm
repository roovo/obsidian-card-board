module Helpers.TaskHelpers exposing (taskId)

import FNV1a


taskId : String -> Int -> String
taskId filePath row =
    String.fromInt (FNV1a.hash filePath) ++ ":" ++ String.fromInt row
