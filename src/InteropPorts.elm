port module InteropPorts exposing
    ( addHoverToCardEditButtons
    , closeView
    , decodeFlags
    , deleteTask
    , displayTaskMarkdown
    , elmInitialized
    , openTaskSourceFile
    , requestFilterCandidates
    , rewriteTasks
    , toElm
    , updateSettings
    )

import BoardConfig exposing (BoardConfig)
import Card exposing (Card)
import CardBoardSettings exposing (GlobalSettings)
import InteropDefinitions
import Json.Decode
import Json.Encode
import SafeZipper exposing (SafeZipper)
import Semver
import TaskItem exposing (TaskItem)
import TimeWithZone exposing (TimeWithZone)
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode


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


elmInitialized : Cmd msg
elmInitialized =
    encodeVariant "elmInitialized" (TsEncode.object []) ()
        |> interopFromElm


deleteTask : { a | filePath : String, lineNumber : Int, originalText : String } -> Cmd msg
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


openTaskSourceFile : { a | filePath : String, lineNumber : Int, originalText : String } -> Cmd msg
openTaskSourceFile info =
    info
        |> encodeVariant "openTaskSourceFile" InteropDefinitions.openTaskSourceFileEncoder
        |> interopFromElm


requestFilterCandidates : Cmd msg
requestFilterCandidates =
    encodeVariant "requestFilterCandidates" (TsEncode.object []) ()
        |> interopFromElm


rewriteTasks : TimeWithZone -> String -> List TaskItem -> Cmd msg
rewriteTasks timeWithZone filePath taskItems =
    let
        rewriteDetails taskItem =
            { lineNumber = TaskItem.lineNumber taskItem
            , originalText = TaskItem.originalText taskItem
            , newText = taskItem |> TaskItem.toggleCompletion timeWithZone |> TaskItem.toString
            }
                |> Debug.log "rewrite details"
    in
    { filePath = filePath, tasks = List.map rewriteDetails taskItems }
        |> encodeVariant "updateTasks" InteropDefinitions.updateTasksEncoder
        |> interopFromElm


updateSettings : SafeZipper BoardConfig -> Cmd msg
updateSettings boardConfigs =
    { version = CardBoardSettings.currentVersion
    , boardConfigs = SafeZipper.toList boardConfigs
    , globalSettings = CardBoardSettings.defaultGlobalSettings
    }
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
