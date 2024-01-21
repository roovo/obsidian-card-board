module UpdatedTaskItem exposing
    ( UpdatedTaskItem
    , completionString
    , dueString
    , init
    , lineNumber
    , originalText
    , toString
    , toggleCompletion
    , updateDate
    )

import DataviewTaskCompletion
import Date exposing (Date)
import GlobalSettings
import Regex exposing (Regex)
import Session exposing (TaskCompletionSettings)
import String.Extra as SE
import TaskItem exposing (Completion, TaskItem)
import TimeWithZone exposing (TimeWithZone)



-- TYPES


type UpdatedTaskItem
    = UpdatedTaskItem Change TaskItem


type Change
    = ChangeCompletion TaskCompletionSettings TimeWithZone
    | ChangeDueDate TaskCompletionSettings (Maybe Date)
    | NoChange



-- CONSTRUCTION


init : TaskItem -> UpdatedTaskItem
init =
    UpdatedTaskItem NoChange



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


dueString : TaskCompletionSettings -> Date -> String
dueString taskCompletionSettings date =
    let
        dateStamp : String
        dateStamp =
            Date.toIsoString date
    in
    case taskCompletionSettings.format of
        GlobalSettings.NoCompletion ->
            "@due(" ++ dateStamp ++ ")"

        GlobalSettings.ObsidianCardBoard ->
            "@due(" ++ dateStamp ++ ")"

        GlobalSettings.ObsidianDataview ->
            "[due:: " ++ dateStamp ++ "]"

        GlobalSettings.ObsidianTasks ->
            "📅 " ++ dateStamp


lineNumber : UpdatedTaskItem -> Int
lineNumber (UpdatedTaskItem _ taskItem) =
    TaskItem.lineNumber taskItem


originalText : UpdatedTaskItem -> String
originalText (UpdatedTaskItem _ taskItem) =
    TaskItem.originalText taskItem


toString : UpdatedTaskItem -> String
toString ((UpdatedTaskItem change taskItem) as updatedTaskItem) =
    case change of
        ChangeCompletion taskCompletionSettings now ->
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

        ChangeDueDate taskCompletionSettings newDate ->
            case newDate of
                Nothing ->
                    originalText updatedTaskItem

                Just date ->
                    let
                        insertDueDate : TaskItem -> String -> String
                        insertDueDate t_ taskString =
                            let
                                tagInserter : String -> String
                                tagInserter =
                                    Regex.find blockLinkRegex taskString
                                        |> List.head
                                        |> Maybe.map .index
                                        |> Maybe.withDefault (String.length taskString)
                                        |> SE.insertAt (dueTag taskCompletionSettings date t_)
                            in
                            tagInserter taskString
                    in
                    taskItem
                        |> TaskItem.originalText
                        |> insertDueDate taskItem

        NoChange ->
            originalText updatedTaskItem



-- MODIFICATION


toggleCompletion : TaskCompletionSettings -> TimeWithZone -> UpdatedTaskItem -> UpdatedTaskItem
toggleCompletion taskCompletionSettings now (UpdatedTaskItem change taskItem) =
    case change of
        ChangeCompletion _ _ ->
            UpdatedTaskItem NoChange taskItem

        ChangeDueDate _ _ ->
            UpdatedTaskItem (ChangeCompletion taskCompletionSettings now) taskItem

        NoChange ->
            UpdatedTaskItem (ChangeCompletion taskCompletionSettings now) taskItem


updateDate : TaskCompletionSettings -> Maybe Date -> UpdatedTaskItem -> UpdatedTaskItem
updateDate taskCompletionSettings date (UpdatedTaskItem _ taskItem) =
    UpdatedTaskItem (ChangeDueDate taskCompletionSettings date) taskItem



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


dueTag : TaskCompletionSettings -> Date -> TaskItem -> String
dueTag taskCompletionSettings date t_ =
    case TaskItem.completion t_ of
        TaskItem.Incomplete ->
            dueString taskCompletionSettings date
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
