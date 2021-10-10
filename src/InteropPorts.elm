port module InteropPorts exposing
    ( addHoverToCardEditButtons
    , decodeFlags
    , deleteTodo
    , displayTaskMarkdown
    , fromElm
    , openTodoSourceFile
    , rewriteTodos
    , toElm
    )

import CardBoard
import InteropDefinitions
import Json.Decode
import Json.Encode
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)
import Time
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode


addHoverToCardEditButtons : String -> Time.Posix -> Time.Zone -> TaskList -> Cmd msg
addHoverToCardEditButtons filePath now zone taskList =
    let
        inColumnLinkIds : List String
        inColumnLinkIds =
            taskList
                |> TaskList.tasks
                |> List.map TaskItem.inColumnId
                |> List.map (\id -> id ++ ":editButton")
    in
    { filePath = filePath, ids = inColumnLinkIds }
        |> encodeProVariant "addFilePreviewHovers" InteropDefinitions.addFilePreviewHoversEncoder
        |> interopFromElm


deleteTodo : { filePath : String, lineNumber : Int, originalText : String } -> Cmd msg
deleteTodo info =
    info
        |> encodeProVariant "deleteTodo" InteropDefinitions.deleteTodoEncoder
        |> interopFromElm


openTodoSourceFile : { filePath : String, blockLink : Maybe String, lineNumber : Int, originalText : String } -> Cmd msg
openTodoSourceFile info =
    info
        |> encodeProVariant "openTodoSourceFile" InteropDefinitions.openTodoSourceFileEncoder
        |> interopFromElm


displayTaskMarkdown : String -> Time.Posix -> Time.Zone -> TaskList -> Cmd msg
displayTaskMarkdown filePath now zone taskList =
    let
        taskItems : List TaskItem
        taskItems =
            TaskList.tasks taskList

        markdownWithIds =
            taskItems
                |> List.map markdownWithId
                |> List.append notesWithIds

        markdownWithId t =
            { id = TaskItem.inColumnId t
            , markdown = TaskItem.title t
            }

        notesMarkdownWithId t =
            { id = TaskItem.inColumnId t ++ ":notes"
            , markdown = TaskItem.notes t
            }

        notesWithIds =
            taskItems
                |> List.filter (\t -> TaskItem.hasNotes t)
                |> List.map notesMarkdownWithId
    in
    { filePath = filePath, todoMarkdown = markdownWithIds }
        |> encodeProVariant "displayTodoMarkdown" InteropDefinitions.displayTodoMarkdownEncoder
        |> interopFromElm


rewriteTodos : Time.Posix -> String -> List TaskItem -> Cmd msg
rewriteTodos now filePath taskItems =
    let
        buildFoo taskItem =
            { lineNumber = TaskItem.lineNumber taskItem
            , originalText = TaskItem.originalText taskItem
            , newText = taskItem |> TaskItem.toggleCompletion now |> TaskItem.toString
            }
    in
    { filePath = filePath, todos = List.map buildFoo taskItems }
        |> encodeProVariant "updateTodos" InteropDefinitions.updateTodosEncoder
        |> interopFromElm


encodeProVariant : String -> TsEncode.Encoder arg1 -> arg1 -> Json.Encode.Value
encodeProVariant variantName encoder_ arg1 =
    arg1
        |> (TsEncode.object
                [ TsEncode.required "tag" identity (TsEncode.literal (Json.Encode.string variantName))
                , TsEncode.required "data" identity encoder_
                ]
                |> TsEncode.encoder
           )


fromElm : InteropDefinitions.FromElm -> Cmd msg
fromElm value =
    value
        |> (InteropDefinitions.interop.fromElm |> TsEncode.encoder)
        |> interopFromElm


toElm : Sub (Result Json.Decode.Error InteropDefinitions.ToElm)
toElm =
    (InteropDefinitions.interop.toElm |> TsDecode.decoder)
        |> Json.Decode.decodeValue
        |> interopToElm


decodeFlags : Json.Decode.Value -> Result Json.Decode.Error InteropDefinitions.Flags
decodeFlags flags =
    Json.Decode.decodeValue
        (InteropDefinitions.interop.flags |> TsDecode.decoder)
        flags


port interopFromElm : Json.Encode.Value -> Cmd msg


port interopToElm : (Json.Decode.Value -> msg) -> Sub msg
