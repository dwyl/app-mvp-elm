module Pages.Home exposing (Model, Msg(..), init, subscriptions, toSession, update, view)

import Asset
import Element exposing (..)
import Element.Font exposing (..)
import Page
import Route
import Session exposing (..)



-- Model


type alias Model =
    Session


init : Session -> ( Model, Cmd Msg )
init session =
    ( session, Cmd.none )



-- Update


type Msg
    = GotSession Session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSession session ->
            ( session
            , Route.replaceUrl (Session.navKey model) Route.Home
            )



-- View


view : Model -> Page.PageStructure Msg
view model =
    { title = "Home"
    , content =
        [ layout [] <|
            column [ centerX ]
                [ image [ centerX ] { src = Asset.imagePath Asset.logo, description = "DWYL Logo" }
                , el [ bold, size 30 ] (text "DWYL Application")
                , case model of
                    Session.Guest _ ->
                        link [] { url = Route.routeToString (Route.Auth Nothing), label = text "login/signup" }

                    Session.Session _ person ->
                        column []
                            [ text <| "logged in with: " ++ person.email
                            , link [] { url = Route.routeToString Route.Capture, label = text "captures" }
                            , link [] { url = Route.routeToString Route.Logout, label = text "logout" }
                            ]
                ]
        ]
    }


toSession : Model -> Session
toSession model =
    model


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changeSession GotSession (Session.navKey model)
