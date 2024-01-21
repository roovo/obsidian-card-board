module UpdatedTaskItem exposing
    ( init
    , toString
    , toggleCompletion
    )

import Regex exposing (Regex)
import String.Extra as SE
import TaskItem exposing (Completion, TaskItem)



-- TYPES


type UpdatedTaskItem
    = UpdatedTaskItem Changes TaskItem


type alias Changes =
    { changeCompletion : Bool
    }



-- CONSTRUCTION


init : TaskItem -> UpdatedTaskItem
init =
    UpdatedTaskItem { changeCompletion = False }



-- INFO


toString : UpdatedTaskItem -> String
toString (UpdatedTaskItem changes taskItem) =
    if changes.changeCompletion then
        let
            replaceCheckbox : TaskItem -> String -> String
            replaceCheckbox t_ taskString =
                let
                    checkboxIndex : Int
                    checkboxIndex =
                        Regex.find checkboxRegex taskString
                            |> List.head
                            |> Maybe.map .index
                            |> Maybe.withDefault 0
                            |> (+) 2
                in
                SE.replaceSlice
                    (toggledCheckbox t_)
                    checkboxIndex
                    (checkboxIndex + 3)
                    taskString
        in
        taskItem
            |> TaskItem.originalText
            |> replaceCheckbox taskItem

    else
        taskItem
            |> TaskItem.originalText



-- MODIFICATION


toggleCompletion : UpdatedTaskItem -> UpdatedTaskItem
toggleCompletion (UpdatedTaskItem changes taskItem) =
    UpdatedTaskItem { changes | changeCompletion = not changes.changeCompletion } taskItem



-- PRIVATE


checkboxRegex : Regex
checkboxRegex =
    Maybe.withDefault Regex.never <|
        Regex.fromString "(-|\\*|\\+) \\[[ xX]\\]"


toggledCheckbox : TaskItem -> String
toggledCheckbox t_ =
    case TaskItem.completion t_ of
        TaskItem.Incomplete ->
            "[x]"

        _ ->
            "[ ]"



--
--
--
--
--
-- toToggledString : DataviewTaskCompletion -> TaskCompletionSettings -> TimeWithZone -> TaskItem -> String
-- toToggledString dataviewTaskCompletion taskCompletionSettings timeWithZone ((TaskItem fields_ _) as taskItem) =
--     let
--         blockLinkRegex : Regex
--         blockLinkRegex =
--             Maybe.withDefault Regex.never <|
--                 Regex.fromString "(\\s\\^[a-zA-Z\\d-]+)$"
--
--         checkboxRegex : Regex
--         checkboxRegex =
--             Maybe.withDefault Regex.never <|
--                 Regex.fromString "(-|\\*|\\+) \\[[ xX]\\]"
--
--         completionTag : TaskItem -> String
--         completionTag t_ =
--             case completion t_ of
--                 Incomplete ->
--                     TimeWithZone.completionString
--                         dataviewTaskCompletion
--                         taskCompletionSettings
--                         timeWithZone
--                         |> (\str ->
--                                 if String.length str == 0 then
--                                     ""
--
--                                 else
--                                     " " ++ str
--                            )
--
--                 _ ->
--                     ""
--
--         insertCompletionTag : TaskItem -> String -> String
--         insertCompletionTag t_ taskString =
--             let
--                 tagInserter : String -> String
--                 tagInserter =
--                     Regex.find blockLinkRegex taskString
--                         |> List.head
--                         |> Maybe.map .index
--                         |> Maybe.withDefault (String.length taskString)
--                         |> SE.insertAt (completionTag t_)
--             in
--             tagInserter taskString
--
--         regexReplacer : String -> (Regex.Match -> String) -> String -> String
--         regexReplacer regex replacer original =
--             case Regex.fromString regex of
--                 Just r ->
--                     Regex.replace r replacer original
--
--                 Nothing ->
--                     original
--
--         removeCompletionTags : String -> String
--         removeCompletionTags =
--             let
--                 dataviewRemover : String -> String
--                 dataviewRemover =
--                     case dataviewTaskCompletion of
--                         DataviewTaskCompletion.NoCompletion ->
--                             identity
--
--                         DataviewTaskCompletion.Emoji ->
--                             identity
--
--                         DataviewTaskCompletion.Text t ->
--                             regexReplacer (" \\[" ++ t ++ ":: \\d{4}-\\d{2}-\\d{2}\\]") (\_ -> "")
--             in
--             regexReplacer " @completed\\(\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(?:(?:[+-]\\d{2}:\\d{2})|Z){0,1}\\)" (\_ -> "")
--                 >> regexReplacer " ✅ \\d{4}-\\d{2}-\\d{2}" (\_ -> "")
--                 >> dataviewRemover
--
--         replaceCheckbox : TaskItem -> String -> String
--         replaceCheckbox t_ taskString =
--             let
--                 checkboxIndex : Int
--                 checkboxIndex =
--                     Regex.find checkboxRegex taskString
--                         |> List.head
--                         |> Maybe.map .index
--                         |> Maybe.withDefault 0
--                         |> (+) 2
--             in
--             SE.replaceSlice
--                 (toggledCheckbox t_)
--                 checkboxIndex
--                 (checkboxIndex + 3)
--                 taskString
--
--         toggledCheckbox : TaskItem -> String
--         toggledCheckbox t_ =
--             case completion t_ of
--                 Incomplete ->
--                     "[x]"
--
--                 _ ->
--                     "[ ]"
--     in
--     fields_.originalText
--         |> replaceCheckbox taskItem
--         |> removeCompletionTags
--         |> insertCompletionTag taskItem
--
