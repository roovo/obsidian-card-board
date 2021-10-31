port module InteropPorts exposing
    ( addHoverToCardEditButtons
    , decodeFlags
    , deleteTodo
    , displayTaskMarkdown
    , openTodoSourceFile
    , rewriteTodos
    , toElm
    , updateSettings
    )

import Card exposing (Card)
import CardBoard
import InteropDefinitions
import Json.Decode
import Json.Encode
import SafeZipper exposing (SafeZipper)
import Semver
import TaskItem exposing (TaskItem)
import TimeWithZone exposing (TimeWithZone)
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode


currentSettingsVersion : Semver.Version
currentSettingsVersion =
    Semver.version 0 1 0 [] []


addHoverToCardEditButtons : List Card -> Cmd msg
addHoverToCardEditButtons cards =
    cards
        |> List.map (\c -> { filePath = Card.filePath c, id = Card.editButtonId c })
        |> encodeVariant "addFilePreviewHovers" InteropDefinitions.addFilePreviewHoversEncoder
        |> interopFromElm


deleteTodo : { filePath : String, lineNumber : Int, originalText : String } -> Cmd msg
deleteTodo info =
    info
        |> encodeVariant "deleteTodo" InteropDefinitions.deleteTodoEncoder
        |> interopFromElm


openTodoSourceFile : { filePath : String, lineNumber : Int, originalText : String } -> Cmd msg
openTodoSourceFile info =
    info
        |> encodeVariant "openTodoSourceFile" InteropDefinitions.openTodoSourceFileEncoder
        |> interopFromElm


displayTaskMarkdown : List Card -> Cmd msg
displayTaskMarkdown cards =
    cards
        |> List.map (\c -> { filePath = Card.filePath c, todoMarkdown = Card.markdownWithIds c })
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


updateSettings : SafeZipper CardBoard.Config -> Cmd msg
updateSettings configs =
    { version = currentSettingsVersion, boardConfigs = SafeZipper.toList configs }
        -- SafeZipper.toList configs
        |> encodeVariant "updateSettings" InteropDefinitions.updateSettingsEncoder
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
