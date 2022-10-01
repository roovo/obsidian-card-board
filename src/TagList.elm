module TagList exposing
    ( TagList
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


toString : TagList -> String
toString (TagList tags) =
    tags
        |> List.map Tag.toString
        |> List.map (String.append "#")
        |> String.join ""
