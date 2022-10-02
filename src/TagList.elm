module TagList exposing
    ( TagList
    , append
    , cons
    , containsTagMatching
    , containsTagMatchingOneOf
    , empty
    , fromList
    , isEmpty
    , toString
    )

import Parser
import Tag exposing (Tag)


type TagList
    = TagList (List Tag)


empty : TagList
empty =
    TagList []


fromList : List String -> TagList
fromList cs =
    let
        buildTag : String -> Maybe Tag
        buildTag content =
            ("#" ++ content)
                |> Parser.run Tag.parser
                |> Result.toMaybe

        pushTag : Maybe Tag -> TagList -> TagList
        pushTag tag list =
            tag
                |> Maybe.map (\t -> cons t list)
                |> Maybe.withDefault list
    in
    cs
        |> List.reverse
        |> List.foldl (\c ts -> pushTag (buildTag c) ts) empty


cons : Tag -> TagList -> TagList
cons tag (TagList tags) =
    TagList (tag :: tags)


append : TagList -> TagList -> TagList
append (TagList l1) (TagList l2) =
    TagList (List.append l1 l2)


isEmpty : TagList -> Bool
isEmpty (TagList ts) =
    List.isEmpty ts


containsTagMatchingOneOf : List String -> TagList -> Bool
containsTagMatchingOneOf candidates tagList =
    List.any (\c -> containsTagMatching c tagList) candidates


containsTagMatching : String -> TagList -> Bool
containsTagMatching candidate (TagList ts) =
    let
        matches : String -> Bool
        matches t =
            if String.endsWith "/" candidate then
                String.startsWith (String.toLower candidate) (String.toLower t)
                    || (String.toLower t == String.dropRight 1 (String.toLower candidate))

            else
                String.toLower t == String.toLower candidate
    in
    List.any matches <| List.map Tag.toString ts


toString : TagList -> String
toString (TagList tags) =
    tags
        |> List.map Tag.toString
        |> List.map (String.append "#")
        |> String.join " "
