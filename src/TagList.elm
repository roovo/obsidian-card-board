module TagList exposing
    ( TagList
    , append
    , cons
    , containsTagMatching
    , containsTagMatchingOneOf
    , containsTagOtherThanThese
    , empty
    , filter
    , fromList
    , isEmpty
    , sort
    , toString
    , toStrings
    , unique
    )

import List.Extra as LE
import Parser
import Set exposing (Set)
import Tag exposing (Tag)



-- TYPES


type TagList
    = TagList (List Tag)



-- CREATE


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
    List.foldr (\c ts -> pushTag (buildTag c) ts) empty cs


cons : Tag -> TagList -> TagList
cons tag (TagList tags) =
    TagList (tag :: tags)



-- COMBINE


append : TagList -> TagList -> TagList
append (TagList l1) (TagList l2) =
    TagList (List.append l1 l2)



-- TRANSFORM


filter : (Tag -> Bool) -> TagList -> TagList
filter test (TagList ts) =
    TagList <| List.filter test ts



-- UTILITIES


containsTagMatching : String -> TagList -> Bool
containsTagMatching candidate =
    List.any (Tag.matches candidate) << toTags


containsTagMatchingOneOf : List String -> TagList -> Bool
containsTagMatchingOneOf candidates tagList =
    List.any (\c -> containsTagMatching c tagList) candidates


containsTagOtherThanThese : List String -> TagList -> Bool
containsTagOtherThanThese candidates tagList =
    let
        fromTagList : Set String
        fromTagList =
            tagList
                |> toStrings
                |> Set.fromList

        fromCandidates : Set String
        fromCandidates =
            Set.fromList candidates
    in
    Set.diff fromTagList fromCandidates
        |> (not << Set.isEmpty)


isEmpty : TagList -> Bool
isEmpty (TagList ts) =
    List.isEmpty ts


unique : TagList -> TagList
unique =
    fromList << LE.uniqueBy String.toLower << toStrings



-- SORT


sort : TagList -> TagList
sort =
    fromList << List.sort << toStrings



-- CONVERT


toString : TagList -> String
toString =
    String.join " " << List.map (String.append "#") << toStrings


toStrings : TagList -> List String
toStrings (TagList ts) =
    List.map Tag.toString ts



-- PRIVATE


toTags : TagList -> List Tag
toTags (TagList ts) =
    ts
