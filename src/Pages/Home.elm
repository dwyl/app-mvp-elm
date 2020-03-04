module Pages.Home exposing (Model, Msg(..), init, toSession, update, view)

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
    = None


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )



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
                span [ class "tc db" ] [ text <| "logged in with token: " ++ person.email ]
        ]
    }


toSession : Model -> Session
toSession model =
    model
