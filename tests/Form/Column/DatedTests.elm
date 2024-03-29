module Form.Column.DatedTests exposing (suite)

import Column.Dated as DatedColumn
import Expect
import Form.Column.Dated as DatedColumnForm
import Form.SafeDecoder as SD
import Test exposing (..)


suite : Test
suite =
    concat
        [ init
        , safeDecoder
        ]


init : Test
init =
    describe "init"
        [ describe "After"
            [ test "intialises the name" <|
                \() ->
                    DatedColumn.init "foo" (DatedColumn.After 0)
                        |> DatedColumnForm.init
                        |> .name
                        |> Expect.equal "foo"
            , test "intialises the rangeType" <|
                \() ->
                    DatedColumn.init "foo" (DatedColumn.After 0)
                        |> DatedColumnForm.init
                        |> .rangeType
                        |> Expect.equal "After"
            , test "intialises the from" <|
                \() ->
                    DatedColumn.init "foo" (DatedColumn.After 3)
                        |> DatedColumnForm.init
                        |> .from
                        |> Expect.equal "3"
            ]
        , describe "Before"
            [ test "intialises the name" <|
                \() ->
                    DatedColumn.init "foo" (DatedColumn.Before 0)
                        |> DatedColumnForm.init
                        |> .name
                        |> Expect.equal "foo"
            , test "intialises the rangeType" <|
                \() ->
                    DatedColumn.init "foo" (DatedColumn.Before 0)
                        |> DatedColumnForm.init
                        |> .rangeType
                        |> Expect.equal "Before"
            , test "intialises the to" <|
                \() ->
                    DatedColumn.init "foo" (DatedColumn.Before 3)
                        |> DatedColumnForm.init
                        |> .to
                        |> Expect.equal "3"
            ]
        , describe "Between"
            [ test "intialises the name" <|
                \() ->
                    DatedColumn.init "foo" (DatedColumn.Between { from = 2, to = 4 })
                        |> DatedColumnForm.init
                        |> .name
                        |> Expect.equal "foo"
            , test "intialises the rangeType" <|
                \() ->
                    DatedColumn.init "foo" (DatedColumn.Between { from = 2, to = 4 })
                        |> DatedColumnForm.init
                        |> .rangeType
                        |> Expect.equal "Between"
            , test "intialises the from" <|
                \() ->
                    DatedColumn.init "foo" (DatedColumn.Between { from = 2, to = 4 })
                        |> DatedColumnForm.init
                        |> .from
                        |> Expect.equal "2"
            , test "intialises the to" <|
                \() ->
                    DatedColumn.init "foo" (DatedColumn.Between { from = 2, to = 4 })
                        |> DatedColumnForm.init
                        |> .to
                        |> Expect.equal "4"
            ]
        ]


safeDecoder : Test
safeDecoder =
    describe "safeDecoder"
        [ describe "After"
            [ test "decodes a valid After input" <|
                \() ->
                    { name = "foo", rangeType = "After", from = "0", to = "" }
                        |> SD.run DatedColumnForm.safeDecoder
                        |> Expect.equal (Ok <| DatedColumn.init "foo" (DatedColumn.After 0))
            , test "igmores leading and trailing whitespace when decoding a valid After input" <|
                \() ->
                    { name = " foo ", rangeType = " After ", from = " 0 ", to = "" }
                        |> SD.run DatedColumnForm.safeDecoder
                        |> Expect.equal (Ok <| DatedColumn.init "foo" (DatedColumn.After 0))
            , test "decodes with a negative from value" <|
                \() ->
                    { name = "foo", rangeType = "After", from = "-3", to = "" }
                        |> SD.run DatedColumnForm.safeDecoder
                        |> Expect.equal (Ok <| DatedColumn.init "foo" (DatedColumn.After -3))
            , test "defaults to 0 with a 'x' as the from value" <|
                \() ->
                    { name = "foo", rangeType = "After", from = "x", to = "" }
                        |> SD.run DatedColumnForm.safeDecoder
                        |> Expect.equal (Ok <| DatedColumn.init "foo" (DatedColumn.After 0))
            , test "defaults to 0 with a decimal from value" <|
                \() ->
                    { name = "foo", rangeType = "After", from = "1.1", to = "" }
                        |> SD.run DatedColumnForm.safeDecoder
                        |> Expect.equal (Ok <| DatedColumn.init "foo" (DatedColumn.After 0))
            , test "defaults to 0 with an empty from value" <|
                \() ->
                    { name = "foo", rangeType = "After", from = "", to = "" }
                        |> SD.run DatedColumnForm.safeDecoder
                        |> Expect.equal (Ok <| DatedColumn.init "foo" (DatedColumn.After 0))
            , test "defaults to '' with an empty name" <|
                \() ->
                    { name = "", rangeType = "After", from = "0", to = "" }
                        |> SD.run DatedColumnForm.safeDecoder
                        |> Expect.equal (Ok <| DatedColumn.init "" (DatedColumn.After 0))
            ]
        , describe "Before"
            [ test "decodes a valid Before input" <|
                \() ->
                    { name = "foo", rangeType = "Before", from = "", to = "0" }
                        |> SD.run DatedColumnForm.safeDecoder
                        |> Expect.equal (Ok <| DatedColumn.init "foo" (DatedColumn.Before 0))
            , test "ignores leading and trailing whitespace when decoding a valid Before input" <|
                \() ->
                    { name = " foo ", rangeType = " Before ", from = "", to = " 0 " }
                        |> SD.run DatedColumnForm.safeDecoder
                        |> Expect.equal (Ok <| DatedColumn.init "foo" (DatedColumn.Before 0))
            , test "decodes with a negative to value" <|
                \() ->
                    { name = "foo", rangeType = "Before", from = "", to = "-5" }
                        |> SD.run DatedColumnForm.safeDecoder
                        |> Expect.equal (Ok <| DatedColumn.init "foo" (DatedColumn.Before -5))
            , test "defaults to 0 with a 'x' as the to value" <|
                \() ->
                    { name = "foo", rangeType = "Before", from = "", to = "x" }
                        |> SD.run DatedColumnForm.safeDecoder
                        |> Expect.equal (Ok <| DatedColumn.init "foo" (DatedColumn.Before 0))
            , test "defaults to 0 with a decimal to value" <|
                \() ->
                    { name = "foo", rangeType = "Before", from = "", to = "1.1" }
                        |> SD.run DatedColumnForm.safeDecoder
                        |> Expect.equal (Ok <| DatedColumn.init "foo" (DatedColumn.Before 0))
            , test "defaults to 0 with an empty to value" <|
                \() ->
                    { name = "foo", rangeType = "Before", from = "", to = "" }
                        |> SD.run DatedColumnForm.safeDecoder
                        |> Expect.equal (Ok <| DatedColumn.init "foo" (DatedColumn.Before 0))
            , test "defaults to 0 with an empty name" <|
                \() ->
                    { name = "", rangeType = "Before", from = "", to = "0" }
                        |> SD.run DatedColumnForm.safeDecoder
                        |> Expect.equal (Ok <| DatedColumn.init "" (DatedColumn.Before 0))
            ]
        , describe "Between"
            [ test "decodes a valid Between input" <|
                \() ->
                    { name = "foo", rangeType = "Between", from = "0", to = "1" }
                        |> SD.run DatedColumnForm.safeDecoder
                        |> Expect.equal (Ok <| DatedColumn.init "foo" (DatedColumn.Between { from = 0, to = 1 }))
            , test "ignores leading and trailing whitespacw when decoding a valid Between input" <|
                \() ->
                    { name = " foo ", rangeType = " Between ", from = " 0 ", to = " 1 " }
                        |> SD.run DatedColumnForm.safeDecoder
                        |> Expect.equal (Ok <| DatedColumn.init "foo" (DatedColumn.Between { from = 0, to = 1 }))
            , test "HAS NO errors with a negative to and from values" <|
                \() ->
                    { name = "foo", rangeType = "Between", from = "-5", to = "-1" }
                        |> SD.run DatedColumnForm.safeDecoder
                        |> Expect.equal (Ok <| DatedColumn.init "foo" (DatedColumn.Between { from = -5, to = -1 }))
            , test "defaults to 0 with a 'x' as the from value" <|
                \() ->
                    { name = "foo", rangeType = "Between", from = "x", to = "1" }
                        |> SD.run DatedColumnForm.safeDecoder
                        |> Expect.equal (Ok <| DatedColumn.init "foo" (DatedColumn.Between { from = 0, to = 1 }))
            , test "defaults to 0 with a 'x' as the to value" <|
                \() ->
                    { name = "foo", rangeType = "Between", from = "0", to = "x" }
                        |> SD.run DatedColumnForm.safeDecoder
                        |> Expect.equal (Ok <| DatedColumn.init "foo" (DatedColumn.Between { from = 0, to = 0 }))
            , test "defaults to 0 with a decimal from value" <|
                \() ->
                    { name = "foo", rangeType = "Between", from = "1.1", to = "1" }
                        |> SD.run DatedColumnForm.safeDecoder
                        |> Expect.equal (Ok <| DatedColumn.init "foo" (DatedColumn.Between { from = 0, to = 1 }))
            , test "defaults to 0 with an empty from value" <|
                \() ->
                    { name = "foo", rangeType = "Between", from = "", to = "1" }
                        |> SD.run DatedColumnForm.safeDecoder
                        |> Expect.equal (Ok <| DatedColumn.init "foo" (DatedColumn.Between { from = 0, to = 1 }))
            , test "defaults to 0 with an empty to value" <|
                \() ->
                    { name = "foo", rangeType = "Between", from = "0", to = "" }
                        |> SD.run DatedColumnForm.safeDecoder
                        |> Expect.equal (Ok <| DatedColumn.init "foo" (DatedColumn.Between { from = 0, to = 0 }))
            , test "allows an empty name" <|
                \() ->
                    { name = "", rangeType = "Between", from = "0", to = "1" }
                        |> SD.run DatedColumnForm.safeDecoder
                        |> Expect.equal (Ok <| DatedColumn.init "" (DatedColumn.Between { from = 0, to = 1 }))
            ]
        , describe "other ranges"
            [ test "defaults to 'Before' with an empty rangeType" <|
                \() ->
                    { name = "foo", rangeType = "", from = "", to = "1" }
                        |> SD.run DatedColumnForm.safeDecoder
                        |> Expect.equal (Ok <| DatedColumn.init "foo" (DatedColumn.Before 1))
            , test "defaults to 'Before' with an invalid rangeType" <|
                \() ->
                    { name = "foo", rangeType = "xxxx", from = "", to = "" }
                        |> SD.run DatedColumnForm.safeDecoder
                        |> Expect.equal (Ok <| DatedColumn.init "foo" (DatedColumn.Before 0))
            ]
        ]
