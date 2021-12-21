module State exposing
    ( State(..)
    , hasLoaded
    , map
    , withDefault
    )

--TYPES


type State a
    = Waiting
    | Loading a
    | Loaded a


hasLoaded : State a -> Bool
hasLoaded a =
    case a of
        Loaded _ ->
            True

        _ ->
            False


map : (a -> b) -> State a -> State b
map fn state =
    case state of
        Waiting ->
            Waiting

        Loading content ->
            Loading (fn content)

        Loaded content ->
            Loaded (fn content)


withDefault : a -> State a -> a
withDefault default state =
    case state of
        Waiting ->
            default

        Loading content ->
            content

        Loaded content ->
            content
