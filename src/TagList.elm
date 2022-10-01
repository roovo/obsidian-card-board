module TagList exposing
    ( TagList
    , append
    , empty
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


toString : TagList -> String
toString (TagList tags) =
    tags
        |> List.map Tag.toString
        |> List.map (String.append "#")
        |> List.reverse
        |> String.join " "
