module TagBoardConfig exposing
    ( ColumnConfig
    , TagBoardConfig
    , columnConfigsParser
    , decoder_v_0_1_0
    , decoder_v_0_2_0
    , decoder_v_0_3_0
    , decoder_v_0_4_0
    , decoder_v_0_5_0
    , default
    , encoder
    )

import Filter exposing (Filter, Polarity, Scope)
import Parser as P exposing ((|.), (|=), Parser)
import ParserHelper
import String.Extra as SE
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode



-- TYPES


type alias TagBoardConfig =
    { columns : List ColumnConfig
    , showColumnTags : Bool
    , completedCount : Int
    , filters : List Filter
    , filterPolarity : Polarity
    , filterScope : Scope
    , showFilteredTags : Bool
    , includeOthers : Bool
    , includeUntagged : Bool
    , title : String
    }


type alias ColumnConfig =
    { tag : String
    , displayTitle : String
    }


default : TagBoardConfig
default =
    { columns = []
    , showColumnTags = True
    , completedCount = 10
    , filters = []
    , filterPolarity = Filter.defaultPolarity
    , filterScope = Filter.defaultScope
    , showFilteredTags = True
    , includeOthers = False
    , includeUntagged = False
    , title = ""
    }



-- SERIALIZE


encoder : TsEncode.Encoder TagBoardConfig
encoder =
    TsEncode.object
        [ TsEncode.required "columns" .columns <| TsEncode.list columnConfigEncoder
        , TsEncode.required "showColumnTags" .showColumnTags TsEncode.bool
        , TsEncode.required "completedCount" .completedCount TsEncode.int
        , TsEncode.required "filters" .filters <| TsEncode.list Filter.encoder
        , TsEncode.required "filterPolarity" .filterPolarity Filter.polarityEncoder
        , TsEncode.required "filterScope" .filterScope Filter.scopeEncoder
        , TsEncode.required "showFilteredTags" .showFilteredTags TsEncode.bool
        , TsEncode.required "includeOthers" .includeOthers TsEncode.bool
        , TsEncode.required "includeUntagged" .includeUntagged TsEncode.bool
        , TsEncode.required "title" .title TsEncode.string
        ]


columnConfigEncoder : TsEncode.Encoder ColumnConfig
columnConfigEncoder =
    TsEncode.object
        [ TsEncode.required "tag" .tag TsEncode.string
        , TsEncode.required "displayTitle" .displayTitle TsEncode.string
        ]


decoder_v_0_5_0 : TsDecode.Decoder TagBoardConfig
decoder_v_0_5_0 =
    TsDecode.succeed TagBoardConfig
        |> TsDecode.andMap (TsDecode.field "columns" (TsDecode.list columnConfigDecoder))
        |> TsDecode.andMap (TsDecode.field "showColumnTags" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "completedCount" TsDecode.int)
        |> TsDecode.andMap (TsDecode.field "filters" <| TsDecode.list Filter.decoder)
        |> TsDecode.andMap (TsDecode.field "filterPolarity" <| Filter.polarityDecoder)
        |> TsDecode.andMap (TsDecode.field "filterScope" <| Filter.scopeDecoder)
        |> TsDecode.andMap (TsDecode.field "showFilteredTags" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "includeOthers" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "includeUntagged" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "title" TsDecode.string)


decoder_v_0_4_0 : TsDecode.Decoder TagBoardConfig
decoder_v_0_4_0 =
    TsDecode.succeed TagBoardConfig
        |> TsDecode.andMap (TsDecode.field "columns" (TsDecode.list columnConfigDecoder))
        |> TsDecode.andMap (TsDecode.field "showColumnTags" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "completedCount" TsDecode.int)
        |> TsDecode.andMap (TsDecode.field "filters" <| TsDecode.list Filter.decoder)
        |> TsDecode.andMap (TsDecode.field "filterPolarity" <| Filter.polarityDecoder)
        |> TsDecode.andMap (TsDecode.succeed Filter.Both)
        |> TsDecode.andMap (TsDecode.field "showFilteredTags" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "includeOthers" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "includeUntagged" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "title" TsDecode.string)


decoder_v_0_3_0 : TsDecode.Decoder TagBoardConfig
decoder_v_0_3_0 =
    TsDecode.succeed TagBoardConfig
        |> TsDecode.andMap (TsDecode.field "columns" (TsDecode.list columnConfigDecoder))
        |> TsDecode.andMap (TsDecode.succeed True)
        |> TsDecode.andMap (TsDecode.field "completedCount" TsDecode.int)
        |> TsDecode.andMap (TsDecode.field "filters" <| TsDecode.list Filter.decoder)
        |> TsDecode.andMap (TsDecode.field "filterPolarity" <| Filter.polarityDecoder)
        |> TsDecode.andMap (TsDecode.succeed Filter.Both)
        |> TsDecode.andMap (TsDecode.succeed True)
        |> TsDecode.andMap (TsDecode.field "includeOthers" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "includeUntagged" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "title" TsDecode.string)


decoder_v_0_2_0 : TsDecode.Decoder TagBoardConfig
decoder_v_0_2_0 =
    TsDecode.succeed TagBoardConfig
        |> TsDecode.andMap (TsDecode.field "columns" (TsDecode.list columnConfigDecoder))
        |> TsDecode.andMap (TsDecode.succeed True)
        |> TsDecode.andMap (TsDecode.field "completedCount" TsDecode.int)
        |> TsDecode.andMap (TsDecode.field "filters" <| TsDecode.list Filter.decoder)
        |> TsDecode.andMap (TsDecode.succeed Filter.Allow)
        |> TsDecode.andMap (TsDecode.succeed Filter.Both)
        |> TsDecode.andMap (TsDecode.succeed True)
        |> TsDecode.andMap (TsDecode.field "includeOthers" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "includeUntagged" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "title" TsDecode.string)


decoder_v_0_1_0 : TsDecode.Decoder TagBoardConfig
decoder_v_0_1_0 =
    TsDecode.succeed TagBoardConfig
        |> TsDecode.andMap (TsDecode.field "columns" (TsDecode.list columnConfigDecoder))
        |> TsDecode.andMap (TsDecode.succeed True)
        |> TsDecode.andMap (TsDecode.field "completedCount" TsDecode.int)
        |> TsDecode.andMap (TsDecode.succeed [])
        |> TsDecode.andMap (TsDecode.succeed Filter.Allow)
        |> TsDecode.andMap (TsDecode.succeed Filter.Both)
        |> TsDecode.andMap (TsDecode.succeed True)
        |> TsDecode.andMap (TsDecode.field "includeOthers" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "includeUntagged" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "title" TsDecode.string)


columnConfigDecoder : TsDecode.Decoder ColumnConfig
columnConfigDecoder =
    TsDecode.succeed ColumnConfig
        |> TsDecode.andMap (TsDecode.field "tag" TsDecode.string)
        |> TsDecode.andMap (TsDecode.field "displayTitle" TsDecode.string)



-- PARSE


columnConfigsParser : Parser (List ColumnConfig)
columnConfigsParser =
    P.loop [] columnConfigHelp



-- PRIVATE


columnConfigHelp : List ColumnConfig -> Parser (P.Step (List ColumnConfig) (List ColumnConfig))
columnConfigHelp revStmts =
    P.oneOf
        [ P.succeed (\stmt -> P.Loop (stmt :: revStmts))
            |= columnConfigParser
            |. P.spaces
        , P.succeed ()
            |> P.map (\_ -> P.Done (List.reverse revStmts))
        ]


columnConfigParser : Parser ColumnConfig
columnConfigParser =
    let
        buildColumnConfig : ( String, Maybe String ) -> Parser ColumnConfig
        buildColumnConfig ( tag, title ) =
            let
                cleanedTag : String
                cleanedTag =
                    if String.startsWith "#" tag then
                        String.dropLeft 1 tag

                    else
                        tag

                displayTitle : String
                displayTitle =
                    title
                        |> Maybe.withDefault defaultTitle
                        |> String.words
                        |> String.join " "

                defaultTitle : String
                defaultTitle =
                    cleanedTag
                        |> String.replace "/" " "
                        |> SE.toSentenceCase
            in
            P.succeed { tag = cleanedTag, displayTitle = displayTitle }
    in
    P.succeed Tuple.pair
        |. ParserHelper.spaces
        |= ParserHelper.wordParser
        |. ParserHelper.spaces
        |= P.oneOf
            [ P.map Just ParserHelper.nonEmptyStringParser
            , P.succeed Nothing
            ]
        |> P.andThen buildColumnConfig
