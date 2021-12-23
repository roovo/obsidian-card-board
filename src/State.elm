module State exposing
    ( State(..)
    , map
    , withDefault
    )

--TYPES


type State a
    = Waiting
    | Loading a
    | Loaded a


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
