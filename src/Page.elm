module Page exposing (view)

import Html exposing (Html)
import Html.Attributes exposing (class)


view : { content : Html msg, modal : Maybe (Html msg) } -> Html msg
view { content, modal } =
    Html.div [ class "card-board-view" ]
        [ Html.div [ class "card-board-container" ]
            [ content
            , modal |> Maybe.withDefault (Html.text "")
            ]
        ]
