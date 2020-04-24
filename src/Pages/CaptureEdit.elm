module Pages.CaptureEdit exposing (Model, Msg(..), init, subscriptions, toSession, update, view)

import Asset exposing (..)
import Element exposing (..)
import Element.Font exposing (..)
import Page
import Route
import Session exposing (..)
import UI.UI as UI



-- Model


type alias Model =
    Session


init : Session -> Int -> ( Model, Cmd Msg )
init session idCapture =
    ( session, Cmd.none )



-- Update


type Msg
    = GotSession Session


update : Msg -> Model -> ( Model, Cmd msg )
update msg _ =
    case msg of
        GotSession session ->
            if Session.isGuest session then
                ( session
                , Route.replaceUrl (Session.navKey session) Route.Login
                )

            else
                ( session
                , Route.replaceUrl (Session.navKey session) Route.Capture
                )



-- View


view : Model -> Page.PageStructure Msg
view _ =
    { title = "Edit Capture"
    , content =
        [ layout [ family [ typeface "Montserrat", sansSerif ] ] <|
            column [ centerX, spacing 20 ]
                [ UI.dwylLogo
                , text "This is the capture edit page"
                ]
        ]
    }


toSession : Model -> Session
toSession model =
    model


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changeSession GotSession (Session.navKey model)
