module Helpers.TaskListHelpers exposing
    ( exampleDateBoardTaskList
    , exampleTagBoardTaskList
    , parsedTasks
    , taskListFromFile
    , taskListFromFileA
    , taskListFromFileG
    , taskListFromNewFile
    )

import Helpers.DateTimeHelpers as DateTimeHelpers
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


exampleDateBoardTaskList : TaskList
exampleDateBoardTaskList =
    exampleDateBoardTasks
        |> List.map parsedTasks
        |> TaskList.concat


exampleTagBoardTaskList : TaskList
exampleTagBoardTaskList =
    exampleTagBoardTasks
        |> List.map parsedTasks
        |> TaskList.concat



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


exampleDateBoardTasks : List ( String, Maybe String, String )
exampleDateBoardTasks =
    [ undatedTasks
    , moreUndatedTasks
    , ( "d", Just DateTimeHelpers.farFuture, """
- [ ] zapping into the future
- [ ] far future incomplete
- [x] far future complete
""" )
    , ( "e", Just DateTimeHelpers.future, """
- [ ] future incomplete
- [x] future complete @completed(2020-06-02)
""" )
    , ( "c", Just DateTimeHelpers.tomorrow, """
- [ ] tomorrow incomplete
- [ ] a task for tomorrow
- [x] tomorrow complete @completed(2020-06-02)
""" )
    , ( "b", Just DateTimeHelpers.today, """
- [ ] today incomplete
- [x] today complete @completed(2020-06-02)
""" )
    , yesterdaysTasks
    , ( "f", Just "invalid date", """
- [ ] invalid date incomplete #aTag
- [x] invalid date complete
""" )
    ]


undatedTasks : ( String, Maybe String, String )
undatedTasks =
    ( "gg/xx/yy.md", Nothing, """
- [ ] an undated incomplete #bTag
- [ ] incomplete with cTag #cTag
- [ ] untagged incomplete
- [x] undated complete @completed(2020-06-02)
""" )


moreUndatedTasks : ( String, Maybe String, String )
moreUndatedTasks =
    ( "x", Nothing, """
- [ ] more undated incomplete #bTag
- [x] more undated complete @completed(2020-06-03)
""" )


yesterdaysTasks : ( String, Maybe String, String )
yesterdaysTasks =
    ( "a", Just DateTimeHelpers.yesterday, """
- [ ] yesterday incomplete
- [ ] another yesterday incomplete
- [x] yesterday complete @completed(2020-06-01)
""" )


exampleTagBoardTasks : List ( String, Maybe String, String )
exampleTagBoardTasks =
    [ ( "a", Nothing, """
- [ ] a.untagged
- [ ] a.tag1 #tag1
- [ ] a.tag2 #tag2
- [ ] a.tag3 #tag3
- [x] a.tag4 #tag4
""" )
    , ( "b", Nothing, """
- [ ] b.untagged
- [ ] b.tag1 #tag1
- [ ] b.tag2 #tag2
- [ ] b.tag3 #tag3
- [x] b.tag4 #tag4
""" )
    , ( "aa/bb/c", Nothing, """
- [ ] c.untagged
- [ ] c.tag1 #tag1
- [ ] c.tag2 #tag2
- [ ] c.tag3 #tag3
- [x] c.tag4 #tag4
""" )
    ]
