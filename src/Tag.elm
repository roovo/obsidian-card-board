module Tag exposing
    ( Tag
    , containsInvalidCharacters
    , decoder
    , encoder
    , equals
    , matches
    , parser
    , startsWith
    , toString
    )

import Parser as P exposing ((|.), (|=), Parser)
import ParserHelper
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode
import Unicode



-- TYPES


type Tag
    = Tag String



-- CREATE


parser : Parser Tag
parser =
    P.succeed Tag
        |. P.token "#"
        |= (P.getChompedString (P.chompWhile isValidTagCharacter)
                |> P.andThen (ParserHelper.checkIfEmpty "Tag.parser")
                |> P.andThen (ParserHelper.checkIsNotNumeric "Tag.parser")
           )



-- SERIALISE


decoder : TsDecode.Decoder Tag
decoder =
    TsDecode.map Tag TsDecode.string


encoder : TsEncode.Encoder Tag
encoder =
    TsEncode.map toString TsEncode.string



-- UTILITIES


containsInvalidCharacters : String -> Bool
containsInvalidCharacters tagString =
    not <| String.all isValidTagCharacter tagString


equals : String -> Tag -> Bool
equals checkString (Tag tagString) =
    checkString == tagString


matches : String -> Tag -> Bool
matches checkString (Tag tagString) =
    let
        exactMatch : String -> String -> Bool
        exactMatch check tag =
            if String.endsWith "/" check then
                String.startsWith check tag || (tag == String.dropRight 1 check)

            else
                tag == check
    in
    exactMatch (String.toLower checkString) (String.toLower tagString)


startsWith : String -> Tag -> Bool
startsWith substring (Tag tagString) =
    String.startsWith substring tagString



-- CONVERT


toString : Tag -> String
toString (Tag s) =
    s



-- PRIVATE


isValidTagCharacter : Char -> Bool
isValidTagCharacter c =
    let
        code : Int
        code =
            Char.toCode c
    in
    Char.isAlphaNum c
        || (code == Unicode.minusCode)
        || (code == Unicode.forwardslashCode)
        || (code == Unicode.underscoreCode)
        || (code > Unicode.basicLatinEndCode && not (Unicode.isWhitespace c))
