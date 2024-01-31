module TagListTests exposing (suite)

import Expect
import Parser
import Tag exposing (Tag)
import TagList exposing (TagList)
import Test exposing (..)
import TsJson.Encode as TsEncode


suite : Test
suite =
    concat
        [ append
        , cons
        , containsTagMatching
        , containsTagMatchingOneOf
        , containsTagOtherThanThese
        , empty
        , encoder
        , filter
        , fromList
        , isEmpty
        , sort
        , toStrings
        , unique
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
                TagList.fromList [ "foo", "bar" ]
                    |> TagList.append TagList.empty
                    |> TagList.toString
                    |> Expect.equal "#foo #bar"
        , test "puts two TagLists together" <|
            \() ->
                TagList.fromList [ "baz", "quz" ]
                    |> TagList.append (TagList.fromList [ "foo", "bar" ])
                    |> TagList.toString
                    |> Expect.equal "#foo #bar #baz #quz"
        ]


cons : Test
cons =
    describe "cons"
        [ test "adds a Tag onto an empty TagList" <|
            \() ->
                TagList.empty
                    |> consTag (buildTag "foo")
                    |> TagList.toString
                    |> Expect.equal "#foo"
        , test "adds Tags to the start of the TagList" <|
            \() ->
                TagList.empty
                    |> consTag (buildTag "foo")
                    |> consTag (buildTag "bar")
                    |> consTag (buildTag "baz")
                    |> Expect.equal (TagList.fromList [ "baz", "bar", "foo" ])
        ]


containsTagMatching : Test
containsTagMatching =
    describe "containsTagMatching"
        [ describe "containsTagMatching - basic"
            [ test "returns True if the TagList contains an exact match" <|
                \() ->
                    TagList.fromList [ "foo", "bar" ]
                        |> TagList.containsTagMatching "foo"
                        |> Expect.equal True
            , test "is case insensative" <|
                \() ->
                    TagList.fromList [ "foO", "bar" ]
                        |> TagList.containsTagMatching "Foo"
                        |> Expect.equal True
            , test "returns False if the TagList does NOT contain the tag" <|
                \() ->
                    TagList.fromList [ "foo", "bar" ]
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
                    TagList.fromList [ "foo", "bar" ]
                        |> TagList.containsTagMatching "fo"
                        |> Expect.equal False
            , test "returns False if the TagList contains the tag but it is followed  by a '/'" <|
                \() ->
                    TagList.fromList [ "foo/", "bar" ]
                        |> TagList.containsTagMatching "foo"
                        |> Expect.equal False
            , test "returns False if the TagList contains the tag but it is followed  by a subtag" <|
                \() ->
                    TagList.fromList [ "foo/baz", "bar" ]
                        |> TagList.containsTagMatching "foo"
                        |> Expect.equal False
            ]
        , describe "containsTagMatching - subtags"
            [ test "returns True if the TagList contains an exact match" <|
                \() ->
                    TagList.fromList [ "foo", "bar/baz" ]
                        |> TagList.containsTagMatching "bar/baz"
                        |> Expect.equal True
            , test "returns False if the TagList contains the tag/subtag but it has a trailing '/'" <|
                \() ->
                    TagList.fromList [ "foo", "bar/baz/" ]
                        |> TagList.containsTagMatching "bar/baz"
                        |> Expect.equal False
            , test "only matches actual subtags" <|
                \() ->
                    TagList.fromList [ "foo", "bart" ]
                        |> TagList.containsTagMatching "bar/"
                        |> Expect.equal False
            ]
        , describe "containsTagMatching - subtag wildcards"
            [ test "returns True if the TagList contains an exact match" <|
                \() ->
                    TagList.fromList [ "foo", "bar/" ]
                        |> TagList.containsTagMatching "bar/"
                        |> Expect.equal True
            , test "returns True if the TagList contains the tag with a subtag" <|
                \() ->
                    TagList.fromList [ "foo", "bar/baz" ]
                        |> TagList.containsTagMatching "bar/"
                        |> Expect.equal True
            ]
        ]


containsTagMatchingOneOf : Test
containsTagMatchingOneOf =
    describe "containsTagMatchingOneOf"
        [ test "returns True if the TagList contains a matching tag" <|
            \() ->
                TagList.fromList [ "foo", "bar" ]
                    |> TagList.containsTagMatchingOneOf [ "bar", "baz" ]
                    |> Expect.equal True
        , test "returns False if the TagList does NOT contain a matching tag" <|
            \() ->
                TagList.fromList [ "foo", "bar" ]
                    |> TagList.containsTagMatchingOneOf [ "baz", "qux" ]
                    |> Expect.equal False
        ]


containsTagOtherThanThese : Test
containsTagOtherThanThese =
    describe "containsTagOtherThanThese - subtag wildcards"
        [ test "returns False if the TagList and the specified tags are both empty" <|
            \() ->
                TagList.fromList []
                    |> TagList.containsTagOtherThanThese []
                    |> Expect.equal False
        , test "returns False if the TagList is empty" <|
            \() ->
                TagList.fromList []
                    |> TagList.containsTagOtherThanThese [ "ytag", "ztag" ]
                    |> Expect.equal False
        , test "returns False if the TagList and the specified tags are the same" <|
            \() ->
                TagList.fromList [ "atag", "btag" ]
                    |> TagList.containsTagOtherThanThese [ "atag", "btag" ]
                    |> Expect.equal False
        , test "returns False if the TagList is a subset of the specified tags" <|
            \() ->
                TagList.fromList [ "atag" ]
                    |> TagList.containsTagOtherThanThese [ "atag", "btag" ]
                    |> Expect.equal False
        , test "returns True if there are no specified tags" <|
            \() ->
                TagList.fromList [ "atag", "btag" ]
                    |> TagList.containsTagOtherThanThese []
                    |> Expect.equal True
        , test "returns True if the specified tags are a subset of the TagList" <|
            \() ->
                TagList.fromList [ "atag", "btag" ]
                    |> TagList.containsTagOtherThanThese [ "btag" ]
                    |> Expect.equal True
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


encoder : Test
encoder =
    describe "encoder"
        [ test "encodes the list as a list of strings" <|
            \() ->
                TagList.fromList [ "atag", "btag" ]
                    |> TsEncode.runExample TagList.encoder
                    |> .output
                    |> Expect.equal "[\"atag\",\"btag\"]"
        ]


filter : Test
filter =
    describe "filter"
        [ test "does nothing if the test is always satisfied" <|
            \() ->
                TagList.fromList [ "foo", "bar", "baz", "qux" ]
                    |> TagList.filter (always True)
                    |> TagList.toString
                    |> Expect.equal "#foo #bar #baz #qux"
        , test "filters matching tags" <|
            \() ->
                TagList.fromList [ "foo", "bar", "baz", "qux" ]
                    |> TagList.filter (Tag.startsWith "ba")
                    |> TagList.toString
                    |> Expect.equal "#bar #baz"
        ]


fromList : Test
fromList =
    describe "fromList"
        [ test "builds an empty TagList from an empty list" <|
            \() ->
                TagList.fromList []
                    |> TagList.isEmpty
                    |> Expect.equal True
        , test "builds the TagList from the list" <|
            \() ->
                TagList.fromList [ "foo", "bar" ]
                    |> TagList.toString
                    |> Expect.equal "#foo #bar"
        , test "ignores invalid tags" <|
            \() ->
                TagList.fromList [ "!@#", "foo", "123", "bar", "", "#plop" ]
                    |> TagList.toString
                    |> Expect.equal "#foo #bar"
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
                TagList.fromList [ "foo", "bar" ]
                    |> TagList.isEmpty
                    |> Expect.equal False
        ]


sort : Test
sort =
    describe "sort"
        [ test "sorts the Tags alphabetically" <|
            \() ->
                TagList.fromList [ "foo", "bar" ]
                    |> TagList.sort
                    |> TagList.toStrings
                    |> Expect.equal [ "bar", "foo" ]
        ]


toStrings : Test
toStrings =
    describe "toStrings"
        [ test "returns the TagList as a list of Strings" <|
            \() ->
                TagList.fromList [ "foo", "bar" ]
                    |> TagList.toStrings
                    |> Expect.equal [ "foo", "bar" ]
        ]


unique : Test
unique =
    describe "unique"
        [ test "removes duplicate tags" <|
            \() ->
                TagList.fromList [ "foo", "foo", "foo", "bar", "bar" ]
                    |> TagList.unique
                    |> TagList.toString
                    |> Expect.equal "#foo #bar"
        , test "is case insensative (uses first occurance)" <|
            \() ->
                TagList.fromList [ "Foo", "foo", "FOO", "bar", "bAr" ]
                    |> TagList.unique
                    |> TagList.toString
                    |> Expect.equal "#Foo #bar"
        ]



-- HELPERS


buildTag : String -> Maybe Tag
buildTag content =
    ("#" ++ content)
        |> Parser.run Tag.parser
        |> Result.toMaybe


consTag : Maybe Tag -> TagList -> TagList
consTag tag list =
    tag
        |> Maybe.map (\t -> TagList.cons t list)
        |> Maybe.withDefault TagList.empty
