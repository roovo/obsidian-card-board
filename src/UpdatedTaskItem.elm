module UpdatedTaskItem exposing
    ( init
    , toString
    , toggleCompletion
    )

import TaskItem exposing (TaskItem)



-- TYPES


type UpdatedTaskItem
    = UpdatedTaskItem TaskItem



-- CONSTRUCTION


init : TaskItem -> UpdatedTaskItem
init =
    UpdatedTaskItem



-- INFO


toString : UpdatedTaskItem -> String
toString (UpdatedTaskItem taskItem) =
    TaskItem.originalText taskItem



-- MODIFICATION


toggleCompletion : UpdatedTaskItem -> UpdatedTaskItem
toggleCompletion updatedTaskItem =
    updatedTaskItem
