module UpdatedTaskItem exposing
    ( UpdatedTaskItem
    , completionString
    , init
    , lineNumber
    , originalText
    , toString
    , toggleCompletion
    )

import DataviewTaskCompletion
import GlobalSettings
import Regex exposing (Regex)
import Session exposing (TaskCompletionSettings)
import String.Extra as SE
import TaskItem exposing (Completion, TaskItem)
import TimeWithZone exposing (TimeWithZone)



-- TYPES


type UpdatedTaskItem
    = UpdatedTaskItem Changes TaskItem


type alias Changes =
    { completionChange : CompletionChange
    }


type CompletionChange
    = NoChange
    | Change TaskCompletionSettings TimeWithZone



-- CONSTRUCTION


init : TaskItem -> UpdatedTaskItem
init =
    UpdatedTaskItem { completionChange = NoChange }



-- INFO


completionString : TaskCompletionSettings -> TimeWithZone -> String
completionString taskCompletionSettings timeWithZone =
    let
        timeStamp : String
        timeStamp =
            TimeWithZone.toString taskCompletionSettings timeWithZone

        dataviewTaskCompletion =
            taskCompletionSettings.dataviewTaskCompletion
    in
    case taskCompletionSettings.format of
        GlobalSettings.NoCompletion ->
            ""

        GlobalSettings.ObsidianCardBoard ->
            "@completed(" ++ timeStamp ++ ")"

        GlobalSettings.ObsidianDataview ->
            case dataviewTaskCompletion of
                DataviewTaskCompletion.NoCompletion ->
                    ""

                DataviewTaskCompletion.Emoji ->
                    "✅ " ++ String.left 10 timeStamp

                DataviewTaskCompletion.Text t ->
                    "[" ++ t ++ ":: " ++ String.left 10 timeStamp ++ "]"

        GlobalSettings.ObsidianTasks ->
            "✅ " ++ String.left 10 timeStamp


lineNumber : UpdatedTaskItem -> Int
lineNumber (UpdatedTaskItem _ taskItem) =
    TaskItem.lineNumber taskItem


originalText : UpdatedTaskItem -> String
originalText (UpdatedTaskItem _ taskItem) =
    TaskItem.originalText taskItem


toString : UpdatedTaskItem -> String
toString (UpdatedTaskItem changes taskItem) =
    case changes.completionChange of
        Change taskCompletionSettings now ->
            let
                insertCompletionTag : TaskItem -> String -> String
                insertCompletionTag t_ taskString =
                    let
                        tagInserter : String -> String
                        tagInserter =
                            Regex.find blockLinkRegex taskString
                                |> List.head
                                |> Maybe.map .index
                                |> Maybe.withDefault (String.length taskString)
                                |> SE.insertAt (completionTag taskCompletionSettings now t_)
                    in
                    tagInserter taskString

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

                regexReplacer : String -> (Regex.Match -> String) -> String -> String
                regexReplacer regex replacer original =
                    case Regex.fromString regex of
                        Just r ->
                            Regex.replace r replacer original

                        Nothing ->
                            original

                removeCompletionTags : String -> String
                removeCompletionTags =
                    let
                        dataviewRemover : String -> String
                        dataviewRemover =
                            case taskCompletionSettings.dataviewTaskCompletion of
                                DataviewTaskCompletion.NoCompletion ->
                                    identity

                                DataviewTaskCompletion.Emoji ->
                                    identity

                                DataviewTaskCompletion.Text t ->
                                    regexReplacer (" \\[" ++ t ++ ":: \\d{4}-\\d{2}-\\d{2}\\]") (\_ -> "")
                    in
                    regexReplacer " @completed\\(\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(?:(?:[+-]\\d{2}:\\d{2})|Z){0,1}\\)" (\_ -> "")
                        >> regexReplacer " ✅ \\d{4}-\\d{2}-\\d{2}" (\_ -> "")
                        >> dataviewRemover
            in
            taskItem
                |> TaskItem.originalText
                |> replaceCheckbox taskItem
                |> removeCompletionTags
                |> insertCompletionTag taskItem

        NoChange ->
            TaskItem.originalText taskItem



-- MODIFICATION


toggleCompletion : TaskCompletionSettings -> TimeWithZone -> UpdatedTaskItem -> UpdatedTaskItem
toggleCompletion taskCompletionSettings now (UpdatedTaskItem changes taskItem) =
    case changes.completionChange of
        NoChange ->
            UpdatedTaskItem
                { changes | completionChange = Change taskCompletionSettings now }
                taskItem

        Change _ _ ->
            UpdatedTaskItem
                { changes | completionChange = NoChange }
                taskItem



-- PRIVATE


blockLinkRegex : Regex
blockLinkRegex =
    Maybe.withDefault Regex.never <|
        Regex.fromString "(\\s\\^[a-zA-Z\\d-]+)$"


checkboxRegex : Regex
checkboxRegex =
    Maybe.withDefault Regex.never <|
        Regex.fromString "(-|\\*|\\+) \\[[ xX]\\]"


completionTag : TaskCompletionSettings -> TimeWithZone -> TaskItem -> String
completionTag taskCompletionSettings now t_ =
    case TaskItem.completion t_ of
        TaskItem.Incomplete ->
            completionString taskCompletionSettings now
                |> (\str ->
                        if String.length str == 0 then
                            ""

                        else
                            " " ++ str
                   )

        _ ->
            ""


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
--     in
--     fields_.originalText
--         |> replaceCheckbox taskItem
--         |> removeCompletionTags
--         |> insertCompletionTag taskItem
--
