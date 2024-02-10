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
import TaskItem exposing (TaskItemFields)
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode
import UpdatedTaskItem exposing (UpdatedTaskItem)



-- FLAGS


decodeFlags : Json.Decode.Value -> Result Json.Decode.Error InteropDefinitions.Flags
decodeFlags flags =
    Json.Decode.decodeValue
        (InteropDefinitions.interop.flags |> TsDecode.decoder)
        flags



-- SUBSCRIPTIONS


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
        |> InteropDefinitions.AddFilePreviewHovers
        |> TsEncode.encoder InteropDefinitions.interop.fromElm
        |> interopFromElm


closeView : Cmd msg
closeView =
    encodeVariant "closeView" (TsEncode.object []) ()
        |> interopFromElm


deleteTask : TaskItemFields -> Cmd msg
deleteTask taskItemFields =
    taskItemFields
        |> InteropDefinitions.DeleteTask
        |> TsEncode.encoder InteropDefinitions.interop.fromElm
        |> interopFromElm


displayTaskMarkdown : List Card -> Cmd msg
displayTaskMarkdown cards =
    cards
        |> List.map (\c -> { filePath = Card.filePath c, taskMarkdown = Card.markdownWithIds c })
        |> InteropDefinitions.DisplayTaskMarkdown
        |> TsEncode.encoder InteropDefinitions.interop.fromElm
        |> interopFromElm


elmInitialized : String -> Cmd msg
elmInitialized uniqueId =
    uniqueId
        |> InteropDefinitions.ElmInitialized
        |> TsEncode.encoder InteropDefinitions.interop.fromElm
        |> interopFromElm


openTaskSourceFile : TaskItemFields -> Cmd msg
openTaskSourceFile taskItemFields =
    taskItemFields
        |> InteropDefinitions.OpenTaskSourceFile
        |> TsEncode.encoder InteropDefinitions.interop.fromElm
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
        |> InteropDefinitions.UpdateTasks
        |> TsEncode.encoder InteropDefinitions.interop.fromElm
        |> interopFromElm


showCardContextMenu : ( Float, Float ) -> String -> Cmd msg
showCardContextMenu clientPos cardId =
    { clientPos = clientPos, cardId = cardId }
        |> InteropDefinitions.ShowCardContextMenu
        |> TsEncode.encoder InteropDefinitions.interop.fromElm
        |> interopFromElm


trackDraggable : String -> Coords -> String -> Cmd msg
trackDraggable dragType clientPos draggableId =
    { dragType = dragType, clientPos = clientPos, draggableId = draggableId }
        |> InteropDefinitions.TrackDraggable
        |> TsEncode.encoder InteropDefinitions.interop.fromElm
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



-- PORTS


port interopFromElm : Json.Encode.Value -> Cmd msg


port interopToElm : (Json.Decode.Value -> msg) -> Sub msg
