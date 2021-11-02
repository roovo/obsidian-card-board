port module InteropPorts exposing
    ( addHoverToCardEditButtons
    , closeView
    , decodeFlags
    , deleteTask
    , displayTaskMarkdown
    , openTaskSourceFile
    , rewriteTasks
    , toElm
    , updateSettings
    )

import BoardConfig exposing (BoardConfig)
import Card exposing (Card)
import CardBoardSettings
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


toElm : Sub (Result Json.Decode.Error InteropDefinitions.ToElm)
toElm =
    (InteropDefinitions.interop.toElm |> TsDecode.decoder)
        |> Json.Decode.decodeValue
        |> interopToElm



-- COMMANDS


addHoverToCardEditButtons : List Card -> Cmd msg
addHoverToCardEditButtons cards =
    cards
        |> List.map (\c -> { filePath = Card.filePath c, id = Card.editButtonId c })
        |> encodeVariant "addFilePreviewHovers" InteropDefinitions.addFilePreviewHoversEncoder
        |> interopFromElm


closeView : Cmd msg
closeView =
    encodeVariant "closeView" (TsEncode.object []) ()
        |> interopFromElm


deleteTask : { filePath : String, lineNumber : Int, originalText : String } -> Cmd msg
deleteTask info =
    info
        |> encodeVariant "deleteTask" InteropDefinitions.deleteTaskEncoder
        |> interopFromElm


displayTaskMarkdown : List Card -> Cmd msg
displayTaskMarkdown cards =
    cards
        |> List.map (\c -> { filePath = Card.filePath c, taskMarkdown = Card.markdownWithIds c })
        |> encodeVariant "displayTaskMarkdown" InteropDefinitions.displayTaskMarkdownEncoder
        |> interopFromElm


openTaskSourceFile : { filePath : String, lineNumber : Int, originalText : String } -> Cmd msg
openTaskSourceFile info =
    info
        |> encodeVariant "openTaskSourceFile" InteropDefinitions.openTaskSourceFileEncoder
        |> interopFromElm


rewriteTasks : TimeWithZone -> String -> List TaskItem -> Cmd msg
rewriteTasks timeWithZone filePath taskItems =
    let
        rewriteDetails taskItem =
            { lineNumber = TaskItem.lineNumber taskItem
            , originalText = TaskItem.originalText taskItem
            , newText = taskItem |> TaskItem.toggleCompletion timeWithZone |> TaskItem.toString
            }
    in
    { filePath = filePath, tasks = List.map rewriteDetails taskItems }
        |> encodeVariant "updateTasks" InteropDefinitions.updateTasksEncoder
        |> interopFromElm


updateSettings : SafeZipper BoardConfig -> Cmd msg
updateSettings configs =
    { version = currentSettingsVersion, boardConfigs = SafeZipper.toList configs }
        |> encodeVariant "updateSettings" CardBoardSettings.encoder
        |> interopFromElm



-- HELPERS


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



-- PORTS


port interopFromElm : Json.Encode.Value -> Cmd msg


port interopToElm : (Json.Decode.Value -> msg) -> Sub msg
