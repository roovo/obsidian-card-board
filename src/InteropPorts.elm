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
import TimeWithZone exposing (TimeWithZone)
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode


addHoverToCardEditButtons : String -> List Card -> Cmd msg
addHoverToCardEditButtons filePath cards =
    { filePath = filePath, ids = List.map Card.editButtonId cards }
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
    { filePath = filePath, todoMarkdown = List.concatMap Card.markdownWithIds cards }
        |> encodeVariant "displayTodoMarkdown" InteropDefinitions.displayTodoMarkdownEncoder
        |> interopFromElm


rewriteTodos : TimeWithZone -> String -> List TaskItem -> Cmd msg
rewriteTodos timeWithZone filePath taskItems =
    let
        rewriteDetails taskItem =
            { lineNumber = TaskItem.lineNumber taskItem
            , originalText = TaskItem.originalText taskItem
            , newText = taskItem |> TaskItem.toggleCompletion timeWithZone |> TaskItem.toString
            }
    in
    { filePath = filePath, todos = List.map rewriteDetails taskItems }
        |> encodeVariant "updateTodos" InteropDefinitions.updateTodosEncoder
        |> interopFromElm


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



-- PRIVATE


encodeVariant : String -> TsEncode.Encoder arg1 -> arg1 -> Json.Encode.Value
encodeVariant variantName encoder_ arg1 =
    arg1
        |> (TsEncode.object
                [ TsEncode.required "tag" identity (TsEncode.literal (Json.Encode.string variantName))
                , TsEncode.required "data" identity encoder_
                ]
                |> TsEncode.encoder
           )


decodeFlags : Json.Decode.Value -> Result Json.Decode.Error InteropDefinitions.Flags
decodeFlags flags =
    Json.Decode.decodeValue
        (InteropDefinitions.interop.flags |> TsDecode.decoder)
        flags


port interopFromElm : Json.Encode.Value -> Cmd msg


port interopToElm : (Json.Decode.Value -> msg) -> Sub msg
