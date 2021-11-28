module Page exposing (view)

import Html exposing (Html)
import Html.Attributes exposing (class)
import Session exposing (Session)


view : Session -> { content : Html msg, modal : Maybe (Html msg) } -> Html msg
view session { content, modal } =
    Html.div [ class "card-board" ]
        [ Html.div [ class "card-board-container" ]
            [ content
            , modal |> Maybe.withDefault (Html.text "")
            ]
        ]
