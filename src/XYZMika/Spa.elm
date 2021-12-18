module XYZMika.Spa exposing
    ( Msg
    , Spa
    , application
    , currentRoute
    , currentUrl
    , pushUrl
    , update
    )

import Browser exposing (Document)
import Browser.Navigation as Navigation exposing (Key)
import Task
import Url exposing (Url)
import XYZMika.Spa.Router as Router exposing (Router)


type Spa route
    = Spa { router : Router route }


type Msg
    = OnUrlRequest Browser.UrlRequest
    | OnUrlChange Url


type alias Config route msg =
    { routeFromUrl : Url -> Maybe route
    , routeToPath : route -> String
    , onRouteChange : Maybe route -> msg
    }


application :
    Config route msg
    ->
        { init : Url.Url -> Navigation.Key -> ( Spa route, Cmd msg )
        , onUrlRequest : Browser.UrlRequest -> Msg
        , onUrlChange : Url -> Msg
        , route : Spa route -> Maybe route
        }
application config =
    { init = init config.onRouteChange config.routeFromUrl
    , onUrlRequest = OnUrlRequest
    , onUrlChange = OnUrlChange
    , route = currentRoute
    }


currentUrl : Spa route -> Url
currentUrl (Spa { router }) =
    Router.currentUrl router


currentRoute : Spa route -> Maybe route
currentRoute (Spa { router }) =
    Router.route router


pushUrl : Spa route -> String -> Cmd msg
pushUrl (Spa spa) url =
    Router.pushUrl spa.router url


init :
    (Maybe route -> msg)
    -> (Url -> Maybe route)
    -> Url
    -> Key
    -> ( Spa route, Cmd msg )
init onRouteChange fromUrl url key =
    ( Spa { router = Router.init url key fromUrl }
    , Task.perform (\_ -> onRouteChange (fromUrl url)) (Task.succeed ())
    )


update :
    Config route msg
    -> Msg
    -> Spa route
    -> ( Spa route, Cmd msg )
update config msg (Spa model) =
    case msg of
        OnUrlRequest request ->
            case request of
                Browser.Internal url ->
                    if Router.currentUrl model.router == url then
                        ( Spa model, Cmd.none )

                    else
                        Router.push config.routeFromUrl config.routeToPath model.router url
                            |> Tuple.mapFirst (\router -> Spa { model | router = router })
                            |> Tuple.mapSecond
                                (\cmd ->
                                    Cmd.batch
                                        [ cmd

                                        --, Task.perform (\_ -> config.onRouteChange newRoute) (Task.succeed ())
                                        ]
                                )

                Browser.External href ->
                    ( Spa model
                    , Navigation.load href
                    )

        OnUrlChange url ->
            ( Spa { model | router = Router.withUrl config.routeFromUrl url model.router }
            , Task.perform (\_ -> config.onRouteChange (config.routeFromUrl url)) (Task.succeed ())
              --, Cmd.none
            )
