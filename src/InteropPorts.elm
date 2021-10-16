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

import Card exposing (Card)
import CardBoard
import InteropDefinitions
import Json.Decode
import Json.Encode
import Panel exposing (Panel)
import Panels exposing (Panels)
import SafeZipper
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)
import Time
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode


addHoverToCardEditButtons : String -> List Card -> Cmd msg
addHoverToCardEditButtons filePath cards =
    let
        ids =
            cards
                |> List.map Card.id
                |> List.map (\i -> i ++ ":editButton")
    in
    { filePath = filePath, ids = ids }
        |> encodeVariant "addFilePreviewHovers" InteropDefinitions.addFilePreviewHoversEncoder
        |> interopFromElm


deleteTodo : { filePath : String, lineNumber : Int, originalText : String } -> Cmd msg
deleteTodo info =
    info
        |> encodeVariant "deleteTodo" InteropDefinitions.deleteTodoEncoder
        |> interopFromElm


openTodoSourceFile : { filePath : String, blockLink : Maybe String, lineNumber : Int, originalText : String } -> Cmd msg
openTodoSourceFile info =
    info
        |> encodeVariant "openTodoSourceFile" InteropDefinitions.openTodoSourceFileEncoder
        |> interopFromElm


displayTaskMarkdown : String -> List Card -> Cmd msg
displayTaskMarkdown filePath cards =
    let
        subtaskMarkdownWithId ( id, subtask ) =
            { id = id
            , markdown = TaskItem.title subtask
            }

        subtasksWithIds =
            cards
                |> List.concatMap Card.subtasks
                |> List.map subtaskMarkdownWithId

        notesMarkdownWithId c =
            { id = Card.id c ++ ":notes"
            , markdown = TaskItem.notes (Card.taskItem c)
            }

        notesWithIds =
            cards
                |> List.filter (\c -> TaskItem.hasNotes (Card.taskItem c))
                |> List.map notesMarkdownWithId

        markdownWithId c =
            { id = Card.id c
            , markdown = TaskItem.title (Card.taskItem c)
            }

        markdownWithIds =
            cards
                |> List.map markdownWithId
                |> List.append subtasksWithIds
                |> List.append notesWithIds
    in
    { filePath = filePath, todoMarkdown = markdownWithIds }
        |> encodeVariant "displayTodoMarkdown" InteropDefinitions.displayTodoMarkdownEncoder
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
        |> encodeVariant "updateTodos" InteropDefinitions.updateTodosEncoder
        |> interopFromElm


encodeVariant : String -> TsEncode.Encoder arg1 -> arg1 -> Json.Encode.Value
encodeVariant variantName encoder_ arg1 =
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
