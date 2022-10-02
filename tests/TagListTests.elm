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
        , containsTagMatchingBasic
        , containsTagMatchingSubtag
        , containsTagMatchingSubtagWildcard
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


containsTagMatchingBasic : Test
containsTagMatchingBasic =
    describe "containsTagMatching - basic"
        [ test "returns True if the TagList contains an exact match" <|
            \() ->
                buildList [ "foo", "bar" ]
                    |> TagList.containsTagMatching "foo"
                    |> Expect.equal True
        , test "is case insensative" <|
            \() ->
                buildList [ "foO", "bar" ]
                    |> TagList.containsTagMatching "Foo"
                    |> Expect.equal True
        , test "returns False if the TagList does NOT contain the tag" <|
            \() ->
                buildList [ "foo", "bar" ]
                    |> TagList.containsTagMatching "baz"
                    |> Expect.equal False
        , test "returns False if the TagList is empty" <|
            \() ->
                TagList.empty
                    |> TagList.containsTagMatching "bar"
                    |> Expect.equal False
        , test "returns False if the TagList and the match are empty" <|
            \() ->
                TagList.empty
                    |> TagList.containsTagMatching ""
                    |> Expect.equal False
        , test "returns False if the TagList contains a tag that starts with the match" <|
            \() ->
                buildList [ "foo", "bar" ]
                    |> TagList.containsTagMatching "fo"
                    |> Expect.equal False
        , test "returns False if the TagList contains the tag but it is followed  by a '/'" <|
            \() ->
                buildList [ "foo/", "bar" ]
                    |> TagList.containsTagMatching "foo"
                    |> Expect.equal False
        , test "returns False if the TagList contains the tag but it is followed  by a subtag" <|
            \() ->
                buildList [ "foo/baz", "bar" ]
                    |> TagList.containsTagMatching "foo"
                    |> Expect.equal False
        ]


containsTagMatchingSubtag : Test
containsTagMatchingSubtag =
    describe "containsTagMatching - subtags"
        [ test "returns True if the TagList contains an exact match" <|
            \() ->
                buildList [ "foo", "bar/baz" ]
                    |> TagList.containsTagMatching "bar/baz"
                    |> Expect.equal True
        , test "returns False if the TagList contains the tag/subtag but it has a trailing '/'" <|
            \() ->
                buildList [ "foo", "bar/baz/" ]
                    |> TagList.containsTagMatching "bar/baz"
                    |> Expect.equal False
        , test "only matches actual subtags" <|
            \() ->
                buildList [ "foo", "bart" ]
                    |> TagList.containsTagMatching "bar/"
                    |> Expect.equal False
        ]


containsTagMatchingSubtagWildcard : Test
containsTagMatchingSubtagWildcard =
    describe "containsTagMatching - subtag wildcards"
        [ test "returns True if the TagList contains an exact match" <|
            \() ->
                buildList [ "foo", "bar/" ]
                    |> TagList.containsTagMatching "bar/"
                    |> Expect.equal True
        , test "returns True if the TagList contains the tag with a subtag" <|
            \() ->
                buildList [ "foo", "bar/baz" ]
                    |> TagList.containsTagMatching "bar/"
                    |> Expect.equal True
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
