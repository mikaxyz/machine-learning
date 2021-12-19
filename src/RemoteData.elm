module RemoteData exposing (RemoteData(..), isLoaded, toMaybe)


type RemoteData error a
    = Suspended
    | Loading
    | Progress Float a
    | Loaded a
    | Error error


toMaybe : RemoteData error a -> Maybe a
toMaybe x =
    case x of
        Loaded data ->
            Just data

        Progress _ data ->
            Just data

        _ ->
            Nothing


isLoaded : RemoteData error a -> Bool
isLoaded x =
    case x of
        Loaded _ ->
            True

        _ ->
            False
