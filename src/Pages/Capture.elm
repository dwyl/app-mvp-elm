module Pages.Capture exposing (Model, Msg(..), init, subscriptions, toSession, update, view)

import Asset
import Html exposing (..)
import Html.Attributes exposing (..)
import Page
import Route
import Session exposing (..)



-- Model


type alias Model =
    { session : Session
    , captures : List Capture
    }


type alias Capture =
    { text : String }


init : Session -> ( Model, Cmd Msg )
init session =
    -- load captures, create command wich fetch the list of captures for the user
    -- create new endpoint in Endpoint
    ( Model session [], Cmd.none )



-- Update


type Msg
    = GotSession Session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSession session ->
            ( { model | session = session }
            , Route.replaceUrl (Session.navKey model.session) Route.Home
            )



-- View


view : Model -> Page.PageStructure Msg
view model =
    { title = "Capture"
    , content =
        [ a [ Route.href Route.Home ] [ img [ Asset.src Asset.logo, class "center db pt2" ] [] ]
        , h1 [ class "tc" ] [ text "Dwyl application" ]
        , case model.session of
            Session.Guest _ ->
                a [ Route.href Route.Home, class "tc db" ] [ text "Not logged in yet!" ]

            Session.Session _ person ->
                div []
                    [ h1 [] [ text "list of captures" ]
                    ]
        ]
    }


toSession : Model -> Session
toSession model =
    model.session


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changeSession GotSession (Session.navKey model.session)
