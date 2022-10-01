module Tag exposing (parser)

import Parser exposing (Parser)


type Tag
    = Tag String


parser : Parser Tag
parser =
    Parser.problem "oops"
