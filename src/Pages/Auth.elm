module Pages.Auth exposing (Model, Msg(..), init, subscriptions, toSession, update, view)

import Asset exposing (..)
import Browser.Navigation as Nav
import Element exposing (..)
import Element.Font exposing (..)
import Endpoint
import Page
import Route
import Session exposing (..)
import UI



-- Model


type alias Model =
    Session


init : Session -> ( Model, Cmd Msg )
init session =
    ( session, Nav.load <| Endpoint.toString Endpoint.authUrls )



-- Update


type Msg
    = GotSession Session


update : Msg -> Model -> ( Model, Cmd msg )
update msg _ =
    case msg of
        GotSession session ->
            ( session
            , Route.replaceUrl (Session.navKey session) Route.Capture
            )



-- View


view : Model -> Page.PageStructure Msg
view _ =
    { title = "Auth"
    , content =
        [ layout [ family [ typeface "Montserrat", sansSerif ] ] <|
            column [ centerX, spacing 20 ]
                [ UI.dwylLogo
                , text "Loading auth service page"
                , el [ centerX ] <| html UI.loaderHtml
                ]
        ]
    }


toSession : Model -> Session
toSession model =
    model


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changeSession GotSession (Session.navKey model)
