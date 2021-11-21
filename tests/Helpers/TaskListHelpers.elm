module Helpers.TaskListHelpers exposing
    ( parsedTasks
    , taskListFromFile
    , taskListFromFileA
    , taskListFromFileG
    , taskListFromNewFile
    )

import Parser
import TaskList exposing (TaskList)


parsedTasks : ( String, Maybe String, String ) -> TaskList
parsedTasks ( p, d, ts ) =
    Parser.run (TaskList.parser p d) ts
        |> Result.withDefault TaskList.empty


taskListFromFile : String -> TaskList
taskListFromFile path =
    path
        |> tasksFromFile
        |> parsedTasks


taskListFromFileA : TaskList
taskListFromFileA =
    parsedTasks tasksFromFileA


taskListFromFileG : TaskList
taskListFromFileG =
    parsedTasks tasksFromFileG


taskListFromNewFile : String -> TaskList
taskListFromNewFile path =
    path
        |> tasksFromNewFile
        |> parsedTasks



-- HELPERS


tasksFromFileG : ( String, Maybe String, String )
tasksFromFileG =
    ( "g", Nothing, """
- [ ] g1
- [x] g2
""" )


tasksFromFileA : ( String, Maybe String, String )
tasksFromFileA =
    ( "a", Nothing, """
- [ ] a1
- [x] a2
""" )


tasksFromFile : String -> ( String, Maybe String, String )
tasksFromFile path =
    ( path, Nothing, """
- [ ] c1
- [x] c2
""" )


tasksFromNewFile : String -> ( String, Maybe String, String )
tasksFromNewFile path =
    ( path, Nothing, """
- [ ] n1
- [x] n2
""" )
