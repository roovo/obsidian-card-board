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
    , showCardContextMenu
    , toElm
    , trackDraggable
    , updateSettings
    )

import Card exposing (Card)
import DragAndDrop.Coords exposing (Coords)
import InteropDefinitions
import Json.Decode
import Json.Encode
import Settings exposing (Settings)
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode
import UpdatedTaskItem exposing (UpdatedTaskItem)


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


elmInitialized : String -> Cmd msg
elmInitialized uniqueId =
    uniqueId
        |> InteropDefinitions.ElmInitialized
        |> TsEncode.encoder InteropDefinitions.interop.fromElm
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


rewriteTasks : String -> List UpdatedTaskItem -> Cmd msg
rewriteTasks filePath updatedTaskItems =
    let
        rewriteDetails : UpdatedTaskItem -> { lineNumber : Int, originalText : String, newText : String }
        rewriteDetails updatedTaskItem =
            { lineNumber = UpdatedTaskItem.lineNumber updatedTaskItem
            , originalText = UpdatedTaskItem.originalText updatedTaskItem
            , newText = UpdatedTaskItem.toString updatedTaskItem
            }
    in
    { filePath = filePath, tasks = List.map rewriteDetails updatedTaskItems }
        |> encodeVariant "updateTasks" InteropDefinitions.updateTasksEncoder
        |> interopFromElm


showCardContextMenu : ( Float, Float ) -> String -> Cmd msg
showCardContextMenu clientPos cardId =
    { clientPos = clientPos, cardId = cardId }
        |> encodeVariant "showCardContextMenu" InteropDefinitions.showCardContextMenuEncoder
        |> interopFromElm


trackDraggable : String -> Coords -> String -> Cmd msg
trackDraggable dragType clientPos draggableId =
    { dragType = dragType, clientPos = clientPos, draggableId = draggableId }
        |> encodeVariant "trackDraggable" InteropDefinitions.trackDraggableEncoder
        |> interopFromElm


updateSettings : Settings -> Cmd msg
updateSettings settings =
    { settings | version = Settings.currentVersion }
        |> encodeVariant "updateSettings" Settings.encoder
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
