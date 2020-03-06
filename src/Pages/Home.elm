module Pages.Home exposing (Model, Msg(..), init, subscriptions, toSession, update, view)

import Asset
import Html exposing (..)
import Html.Attributes exposing (..)
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
        [ a [ Route.href Route.Home ] [ img [ Asset.src Asset.logo, class "center db pt2" ] [] ]
        , h1 [ class "tc" ] [ text "Dwyl application" ]
        , case model of
            Session.Guest _ ->
                a [ Route.href (Route.Auth Nothing), class "tc db" ] [ text "login/signup" ]

            Session.Session _ person ->
                div []
                    [ span [ class "tc db" ] [ text <| "logged in with: " ++ person.email ]
                    , a [ Route.href Route.Capture, class "tc db" ] [ text "capture" ]
                    , a [ Route.href Route.Logout, class "tc db" ] [ text "logout" ]
                    ]
        ]
    }


toSession : Model -> Session
toSession model =
    model


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changeSession GotSession (Session.navKey model)
