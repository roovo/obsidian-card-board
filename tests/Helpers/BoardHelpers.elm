module Helpers.BoardHelpers exposing
    ( defaultDateBoardConfig
    , defaultTagBoardConfig
    , exampleBoardConfig
    , exampleDateBoardConfig
    , exampleDateBoardTaskList
    , exampleTagBoardConfig
    , tasksInColumn
    )

import BoardConfig exposing (BoardConfig)
import DateBoard
import Filter
import Helpers.DateTimeHelpers as DateTimeHelpers
import Parser
import TagBoard
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)


defaultDateBoardConfig : DateBoard.Config
defaultDateBoardConfig =
    { completedCount = 0
    , filters = []
    , includeUndated = False
    , title = "Date Board Title"
    }


exampleBoardConfig : BoardConfig
exampleBoardConfig =
    BoardConfig.TagBoardConfig exampleTagBoardConfig


exampleDateBoardConfig : DateBoard.Config
exampleDateBoardConfig =
    { completedCount = 12
    , filters = [ Filter.PathFilter "a/path", Filter.PathFilter "b/path", Filter.TagFilter "tag1", Filter.TagFilter "tag2" ]
    , includeUndated = False
    , title = "Date Board Title"
    }


exampleDateBoardTaskList : TaskList
exampleDateBoardTaskList =
    exampleRawTasks
        |> List.map parseRawTasks
        |> TaskList.concat


defaultTagBoardConfig : TagBoard.Config
defaultTagBoardConfig =
    { columns = []
    , completedCount = 0
    , filters = []
    , includeOthers = False
    , includeUntagged = False
    , title = "Tag Board Title"
    }


exampleTagBoardConfig : TagBoard.Config
exampleTagBoardConfig =
    { columns = [ { tag = "foo", displayTitle = "bar" } ]
    , completedCount = 6
    , filters = [ Filter.PathFilter "a", Filter.PathFilter "b", Filter.TagFilter "t1", Filter.TagFilter "t2" ]
    , includeOthers = False
    , includeUntagged = True
    , title = "Tag Board Title"
    }


tasksInColumn : String -> List ( String, List TaskItem ) -> List TaskItem
tasksInColumn columnName tasksInColumns =
    tasksInColumns
        |> List.filter (\( c, _ ) -> c == columnName)
        |> List.concatMap Tuple.second



-- PRIVATE


parseRawTasks : ( String, Maybe String, String ) -> TaskList
parseRawTasks ( p, d, ts ) =
    Parser.run (TaskList.parser p d) ts
        |> Result.withDefault TaskList.empty


exampleRawTasks : List ( String, Maybe String, String )
exampleRawTasks =
    [ undatedRawTasks
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
    , yesterdaysRawTasks
    , ( "f", Just "invalid date", """
- [ ] invalid date incomplete
- [x] invalid date complete
""" )
    ]


undatedRawTasks : ( String, Maybe String, String )
undatedRawTasks =
    ( "g", Nothing, """
- [ ] an undated incomplete
- [x] undated complete @completed(2020-06-02)
""" )


yesterdaysRawTasks : ( String, Maybe String, String )
yesterdaysRawTasks =
    ( "a", Just DateTimeHelpers.yesterday, """
- [ ] yesterday incomplete
- [ ] another yesterday incomplete
- [x] yesterday complete @completed(2020-06-01)
""" )
