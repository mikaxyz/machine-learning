module RemoteData exposing (RemoteData(..), isLoaded)


type RemoteData error a
    = Suspended
    | Loading
    | Progress Float
    | Loaded a
    | Error error


isLoaded : RemoteData error a -> Bool
isLoaded x =
    case x of
        Loaded _ ->
            True

        _ ->
            False
