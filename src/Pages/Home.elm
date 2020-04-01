module Pages.Home exposing (Model, Msg(..), init, subscriptions, toSession, update, view)

import Asset
import Element exposing (..)
import Element.Font exposing (..)
import Page
import Route
import Session exposing (..)
import UI



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
        [ layout [ family [ typeface "Montserrat", sansSerif ] ] <|
            column [ width fill, height fill, spacing 30 ]
                [ UI.dwylLogo
                , case model of
                    Session.Guest _ ->
                        link [ centerX, color UI.teal, bold ]
                            { url = Route.routeToString (Route.Auth Nothing)
                            , label = text "login/signup"
                            }

                    Session.Session _ person ->
                        column [ centerX, spacing 30 ]
                            [ text <| "logged in with: " ++ person.email
                            , row [ spacing 30, centerX ]
                                [ link [ color UI.teal, bold ] { url = Route.routeToString Route.Capture, label = text "captures" }
                                , link [ color UI.teal, bold ] { url = Route.routeToString Route.Logout, label = text "logout" }
                                ]
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
