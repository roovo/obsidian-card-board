module TagList exposing
    ( TagList
    , append
    , containsTagMatching
    , empty
    , isEmpty
    , push
    , toString
    )

import Tag exposing (Tag)


type TagList
    = TagList (List Tag)


empty : TagList
empty =
    TagList []


push : Tag -> TagList -> TagList
push tag (TagList tags) =
    TagList (tag :: tags)


append : TagList -> TagList -> TagList
append (TagList l1) (TagList l2) =
    TagList (List.append l2 l1)


isEmpty : TagList -> Bool
isEmpty (TagList ts) =
    List.isEmpty ts


containsTagMatching : String -> TagList -> Bool
containsTagMatching candidate (TagList ts) =
    let
        matches : String -> Bool
        matches t =
            if String.endsWith "/" candidate then
                String.startsWith (String.toLower candidate) t
                    || (t == String.dropRight 1 (String.toLower candidate))

            else
                t == String.toLower candidate
    in
    List.any matches <| List.map Tag.toString ts


toString : TagList -> String
toString (TagList tags) =
    tags
        |> List.map Tag.toString
        |> List.map (String.append "#")
        |> List.reverse
        |> String.join " "
