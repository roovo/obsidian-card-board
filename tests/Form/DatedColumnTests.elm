module Form.DatedColumnTests exposing (suite)

import Column.Dated as DatedColumn
import Expect
import Form.DatedColumn as DatedColumnForm
import Form.Decoder as FD
import Test exposing (..)


suite : Test
suite =
    concat
        [ decoder
        , init
        ]


decoder : Test
decoder =
    describe "decoder"
        [ describe "After"
            [ test "decodes a valid After input" <|
                \() ->
                    { name = "foo", rangeType = "After", from = "0", to = "" }
                        |> FD.run DatedColumnForm.decoder
                        |> Expect.equal (Ok <| DatedColumn.init "foo" (DatedColumn.After 0))
            , test "igmores leading and trailing whitespace when decoding a valid After input" <|
                \() ->
                    { name = " foo ", rangeType = " After ", from = " 0 ", to = "" }
                        |> FD.run DatedColumnForm.decoder
                        |> Expect.equal (Ok <| DatedColumn.init "foo" (DatedColumn.After 0))
            , test "has no errors if the input is valid" <|
                \() ->
                    { name = "foo", rangeType = "After", from = "0", to = "" }
                        |> FD.errors DatedColumnForm.decoder
                        |> Expect.equal []
            , test "errors with a 'x' as the to value" <|
                \() ->
                    { name = "foo", rangeType = "After", from = "x", to = "" }
                        |> FD.errors DatedColumnForm.decoder
                        |> Expect.equal [ DatedColumnForm.RangeValueFromError DatedColumnForm.InvalidInt ]
            , test "errors with a decimal to value" <|
                \() ->
                    { name = "foo", rangeType = "After", from = "1.1", to = "" }
                        |> FD.errors DatedColumnForm.decoder
                        |> Expect.equal [ DatedColumnForm.RangeValueFromError DatedColumnForm.InvalidInt ]
            , test "HAS NO errors with a negative to value" <|
                \() ->
                    { name = "foo", rangeType = "After", from = "-3", to = "" }
                        |> FD.errors DatedColumnForm.decoder
                        |> Expect.equal []
            , test "errors with an empty to value" <|
                \() ->
                    { name = "foo", rangeType = "After", from = "", to = "" }
                        |> FD.errors DatedColumnForm.decoder
                        |> Expect.equal [ DatedColumnForm.RangeFromValueRequired ]
            , test "errors with an empty name" <|
                \() ->
                    { name = "", rangeType = "After", from = "0", to = "" }
                        |> FD.errors DatedColumnForm.decoder
                        |> Expect.equal [ DatedColumnForm.NameRequired ]
            ]
        , describe "Before"
            [ test "decodes a valid Before input" <|
                \() ->
                    { name = "foo", rangeType = "Before", from = "", to = "0" }
                        |> FD.run DatedColumnForm.decoder
                        |> Expect.equal (Ok <| DatedColumn.init "foo" (DatedColumn.Before 0))
            , test "ignores leading and trailing whitespace when decoding a valid Before input" <|
                \() ->
                    { name = " foo ", rangeType = " Before ", from = "", to = " 0 " }
                        |> FD.run DatedColumnForm.decoder
                        |> Expect.equal (Ok <| DatedColumn.init "foo" (DatedColumn.Before 0))
            , test "has no errors if the input is valid" <|
                \() ->
                    { name = "foo", rangeType = "Before", from = "", to = "0" }
                        |> FD.errors DatedColumnForm.decoder
                        |> Expect.equal []
            , test "errors with a 'x' as the from value" <|
                \() ->
                    { name = "foo", rangeType = "Before", from = "", to = "x" }
                        |> FD.errors DatedColumnForm.decoder
                        |> Expect.equal [ DatedColumnForm.RangeValueToError DatedColumnForm.InvalidInt ]
            , test "errors with a decimal from value" <|
                \() ->
                    { name = "foo", rangeType = "Before", from = "", to = "1.1" }
                        |> FD.errors DatedColumnForm.decoder
                        |> Expect.equal [ DatedColumnForm.RangeValueToError DatedColumnForm.InvalidInt ]
            , test "HAS NO errors with a negative from value" <|
                \() ->
                    { name = "foo", rangeType = "Before", from = "", to = "-5" }
                        |> FD.errors DatedColumnForm.decoder
                        |> Expect.equal []
            , test "errors with an empty from value" <|
                \() ->
                    { name = "foo", rangeType = "Before", from = "", to = "" }
                        |> FD.errors DatedColumnForm.decoder
                        |> Expect.equal [ DatedColumnForm.RangeToValueRequired ]
            , test "errors with an empty name" <|
                \() ->
                    { name = "", rangeType = "Before", from = "", to = "0" }
                        |> FD.errors DatedColumnForm.decoder
                        |> Expect.equal [ DatedColumnForm.NameRequired ]
            ]
        , describe "Between"
            [ test "decodes a valid Between input" <|
                \() ->
                    { name = "foo", rangeType = "Between", from = "0", to = "1" }
                        |> FD.run DatedColumnForm.decoder
                        |> Expect.equal (Ok <| DatedColumn.init "foo" (DatedColumn.Between { from = 0, to = 1 }))
            , test "ignores leading and trailing whitespacw when decoding a valid Between input" <|
                \() ->
                    { name = " foo ", rangeType = " Between ", from = " 0 ", to = " 1 " }
                        |> FD.run DatedColumnForm.decoder
                        |> Expect.equal (Ok <| DatedColumn.init "foo" (DatedColumn.Between { from = 0, to = 1 }))
            , test "has no errors if the input is valid" <|
                \() ->
                    { name = "foo", rangeType = "Between", from = "0", to = "1" }
                        |> FD.errors DatedColumnForm.decoder
                        |> Expect.equal []
            , test "errors with a 'x' as the from value" <|
                \() ->
                    { name = "foo", rangeType = "Between", from = "x", to = "1" }
                        |> FD.errors DatedColumnForm.decoder
                        |> Expect.equal [ DatedColumnForm.RangeValueFromError DatedColumnForm.InvalidInt ]
            , test "errors with a 'x' as the to value" <|
                \() ->
                    { name = "foo", rangeType = "Between", from = "0", to = "x" }
                        |> FD.errors DatedColumnForm.decoder
                        |> Expect.equal [ DatedColumnForm.RangeValueToError DatedColumnForm.InvalidInt ]
            , test "errors with a decimal from value" <|
                \() ->
                    { name = "foo", rangeType = "Between", from = "1.1", to = "1" }
                        |> FD.errors DatedColumnForm.decoder
                        |> Expect.equal [ DatedColumnForm.RangeValueFromError DatedColumnForm.InvalidInt ]
            , test "HAS NO errors with a negative to and from values" <|
                \() ->
                    { name = "foo", rangeType = "Between", from = "-5", to = "-1" }
                        |> FD.errors DatedColumnForm.decoder
                        |> Expect.equal []
            , test "errors with an empty from value" <|
                \() ->
                    { name = "foo", rangeType = "Between", from = "", to = "1" }
                        |> FD.errors DatedColumnForm.decoder
                        |> Expect.equal [ DatedColumnForm.RangeFromValueRequired ]
            , test "errors with an empty to value" <|
                \() ->
                    { name = "foo", rangeType = "Between", from = "0", to = "" }
                        |> FD.errors DatedColumnForm.decoder
                        |> Expect.equal [ DatedColumnForm.RangeToValueRequired ]
            , test "errors with an empty name" <|
                \() ->
                    { name = "", rangeType = "Between", from = "0", to = "1" }
                        |> FD.errors DatedColumnForm.decoder
                        |> Expect.equal [ DatedColumnForm.NameRequired ]
            ]
        , describe "other ranges"
            [ test "errors with an empty rangeType" <|
                \() ->
                    { name = "foo", rangeType = "", from = "0", to = "1" }
                        |> FD.errors DatedColumnForm.decoder
                        |> Expect.equal [ DatedColumnForm.RangeTypeRequired ]
            , test "errors with an invalid rangeType" <|
                \() ->
                    { name = "foo", rangeType = "xxxx", from = "0", to = "1" }
                        |> FD.errors DatedColumnForm.decoder
                        |> Expect.equal [ DatedColumnForm.RangeTypeError DatedColumnForm.Invalid ]
            ]
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
