module Form.Input exposing (required)

import Form.Decoder as FD


required : err -> FD.Decoder String err a -> FD.Decoder String err a
required error d =
    FD.with <|
        \a ->
            case String.trim a of
                "" ->
                    FD.fail error

                _ ->
                    FD.lift identity d
