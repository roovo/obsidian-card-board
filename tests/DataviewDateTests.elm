module DataviewDateTests exposing (suite)

import DataviewDate
import DataviewTaskCompletion
import Date
import Expect
import Parser
import Test exposing (..)
import Time


suite : Test
suite =
    concat
        [ completionTimeParser
        , dueDateParser
        ]


completionTimeParser : Test
completionTimeParser =
    describe "completionTimeParser"
        [ test "parsers a valid completion date [completion:: <date>] for DataviewTaskCompletion.NoCompletion" <|
            \() ->
                "[completion:: 2022-10-08]"
                    |> Parser.run (DataviewDate.completionTimeParser DataviewTaskCompletion.NoCompletion identity)
                    |> Expect.equal (Ok <| Time.millisToPosix 1665187200000)
        , test "parsers a valid completion date [completion:: <date>] for DataviewTaskCompletion.Emoji" <|
            \() ->
                "[completion:: 2022-10-08]"
                    |> Parser.run (DataviewDate.completionTimeParser DataviewTaskCompletion.Emoji identity)
                    |> Expect.equal (Ok <| Time.millisToPosix 1665187200000)
        , test "parsers a valid completion date [custom:: <date>] for DataviewTaskCompletion.Text" <|
            \() ->
                "[custom:: 2022-10-08]"
                    |> Parser.run (DataviewDate.completionTimeParser (DataviewTaskCompletion.Text "custom") identity)
                    |> Expect.equal (Ok <| Time.millisToPosix 1665187200000)
        ]


dueDateParser : Test
dueDateParser =
    describe "dueDateParser"
        [ test "parsers a valid due date [due:: <date>]" <|
            \() ->
                "[due:: 2022-10-08]"
                    |> Parser.run (DataviewDate.dueDateParser identity)
                    |> Expect.equal (Ok <| Date.fromRataDie 738436)
        , test "fails if the due date is not valid" <|
            \() ->
                "[due:: 2022-10-32]"
                    |> Parser.run (DataviewDate.dueDateParser identity)
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        , test "fails if the ] has a character immediatedly after it" <|
            \() ->
                "[due:: 2022-10-08]x"
                    |> Parser.run (DataviewDate.dueDateParser identity)
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        , test "fails if it is a completion date" <|
            \() ->
                "[completion:: 2022-10-08]"
                    |> Parser.run (DataviewDate.dueDateParser identity)
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        ]
