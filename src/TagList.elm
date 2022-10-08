module TagList exposing
    ( TagList
    , append
    , cons
    , containsTagMatching
    , containsTagMatchingOneOf
    , empty
    , filter
    , fromList
    , isEmpty
    , sort
    , toList
    , toString
    , unique
    )

import List.Extra as LE
import Parser
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
    let
        matches : String -> Bool
        matches t =
            if String.endsWith "/" candidate then
                String.startsWith (String.toLower candidate) (String.toLower t)
                    || (String.toLower t == String.dropRight 1 (String.toLower candidate))

            else
                String.toLower t == String.toLower candidate
    in
    List.any matches << toList


containsTagMatchingOneOf : List String -> TagList -> Bool
containsTagMatchingOneOf candidates tagList =
    List.any (\c -> containsTagMatching c tagList) candidates


isEmpty : TagList -> Bool
isEmpty (TagList ts) =
    List.isEmpty ts


unique : TagList -> TagList
unique =
    fromList << LE.uniqueBy String.toLower << toList



-- SORT


sort : TagList -> TagList
sort =
    fromList << List.sort << toList



-- CONVERT


toString : TagList -> String
toString =
    String.join " " << List.map (String.append "#") << toList


toList : TagList -> List String
toList (TagList ts) =
    List.map Tag.toString ts
