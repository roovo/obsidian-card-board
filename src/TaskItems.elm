module TaskItems exposing (parser)

import Parser exposing (..)
import TaskItem exposing (TaskItem)


parser : Parser (List TaskItem)
parser =
    loop [] taskItemsHelp


taskItemsHelp : List TaskItem -> Parser (Step (List TaskItem) (List TaskItem))
taskItemsHelp revTaskItems =
    oneOf
        [ TaskItem.parser
            |> map (\taskItem -> Loop (taskItem :: revTaskItems))
        , line
            |> map (\_ -> Loop revTaskItems)
        , succeed ()
            |> map (\_ -> Done (List.reverse revTaskItems))
        ]


line : Parser ()
line =
    succeed ()
        |. nonEmptyStringParser
        |. lineEnd


nonEmptyStringParser : Parser String
nonEmptyStringParser =
    getChompedString chompToEndOfLine
        |> andThen checkIfEmpty


chompToEndOfLine : Parser ()
chompToEndOfLine =
    chompWhile (not << isLineEnd)


checkIfEmpty : String -> Parser String
checkIfEmpty parsedString =
    if String.length parsedString == 0 then
        problem "expecting a title for the Task Item"

    else
        succeed parsedString


isLineEnd : Char -> Bool
isLineEnd char =
    case Char.toCode char of
        10 ->
            -- new line
            True

        13 ->
            -- carriage return
            True

        _ ->
            False


lineEnd : Parser ()
lineEnd =
    chompWhile (\c -> c == '\n' || c == carriageReturn)


carriageReturn : Char
carriageReturn =
    Char.fromCode 13
