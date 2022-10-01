module TagList exposing
    ( empty
    , toString
    )

import Tag exposing (Tag)


type TagList
    = TagList (List Tag)


empty : TagList
empty =
    TagList []


toString : TagList -> String
toString l =
    ""
