module TagListTests exposing (suite)

import Expect
import Parser
import Tag exposing (Tag)
import TagList exposing (TagList)
import Test exposing (..)


suite : Test
suite =
    concat
        [ empty
        , push
        , append
        , isEmpty
        ]


empty : Test
empty =
    describe "empty"
        [ test "contains no Tags" <|
            \() ->
                TagList.empty
                    |> TagList.toString
                    |> Expect.equal ""
        ]


push : Test
push =
    describe "push"
        [ test "can push a Tag onto an empty TagList" <|
            \() ->
                TagList.empty
                    |> pushTag (buildTag "foo")
                    |> TagList.toString
                    |> Expect.equal "#foo"
        , test "can push multiple Tags onto an empty TagList" <|
            \() ->
                TagList.empty
                    |> pushTag (buildTag "foo")
                    |> pushTag (buildTag "bar")
                    |> pushTag (buildTag "baz")
                    |> TagList.toString
                    |> Expect.equal "#foo #bar #baz"
        ]


append : Test
append =
    describe "append"
        [ test "appending two empty TagLists produces and emptu TagList" <|
            \() ->
                TagList.empty
                    |> TagList.append TagList.empty
                    |> TagList.toString
                    |> Expect.equal ""
        , test "appending an empty TagList onto a non-empty one gives the non-empty one" <|
            \() ->
                buildList [ "foo", "bar" ]
                    |> TagList.append TagList.empty
                    |> TagList.toString
                    |> Expect.equal "#foo #bar"
        , test "puts two TagLists together" <|
            \() ->
                buildList [ "baz", "quz" ]
                    |> TagList.append (buildList [ "foo", "bar" ])
                    |> TagList.toString
                    |> Expect.equal "#foo #bar #baz #quz"
        ]


isEmpty : Test
isEmpty =
    describe "isEmpty"
        [ test "returns True for an empty TagList" <|
            \() ->
                TagList.empty
                    |> TagList.isEmpty
                    |> Expect.equal True
        , test "returns False for a non-empty TagList" <|
            \() ->
                buildList [ "foo", "bar" ]
                    |> TagList.isEmpty
                    |> Expect.equal False
        ]



-- HELPERS


buildList : List String -> TagList
buildList cs =
    List.foldl (\c ts -> pushTag (buildTag c) ts) TagList.empty cs


buildTag : String -> Maybe Tag
buildTag content =
    ("#" ++ content)
        |> Parser.run Tag.parser
        |> Result.toMaybe


pushTag : Maybe Tag -> TagList -> TagList
pushTag tag list =
    tag
        |> Maybe.map (\t -> TagList.push t list)
        |> Maybe.withDefault TagList.empty
