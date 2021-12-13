module Helpers.FilterHelpers exposing (exampleFilters)

import Filter exposing (Filter)


exampleFilters : List Filter
exampleFilters =
    [ Filter.PathFilter "a/path"
    , Filter.TagFilter "a_tag"
    , Filter.FileFilter "a/file.md"
    ]
