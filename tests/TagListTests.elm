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
        ]


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
