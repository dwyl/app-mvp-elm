module Main exposing (Page(..), main)

import Asset
import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Page
import Pages.Auth as Auth
import Pages.Home as Home
import Route
import Session
import Url


main : Program (Maybe String) Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- Model


type Page
    = Home Home.Model
    | Auth Auth.Model
    | NotFound Session.Session


type alias Model =
    { key : Nav.Key
    , page : Page
    }



-- flags will contain the session from local storage
-- init look at the url, parse it and load the Page (ie Home, Auth...)


init : Maybe String -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        session =
            case flags of
                Nothing ->
                    Session.Guest

                Just str ->
                    Session.decode str
    in
    loadRoute (Route.fromUrl url) (Model key (NotFound session))



-- UPDATE


type Msg
    = UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | GotHomeMsg Home.Msg
    | GotAuthMsg Auth.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( LinkClicked link, _ ) ->
            case link of
                Browser.Internal urlRequested ->
                    ( model, Nav.pushUrl model.key (Url.toString urlRequested) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( UrlChanged url, _ ) ->
            loadRoute (Route.fromUrl url) model

        ( GotHomeMsg homeMsg, Home homeModel ) ->
            let
                ( subModel, subMsg ) =
                    Home.update homeMsg homeModel
            in
            ( { model | page = Home subModel }, Cmd.map GotHomeMsg subMsg )

        ( GotAuthMsg authMsg, Auth authModel ) ->
            let
                ( subModel, subMsg ) =
                    Auth.update authMsg authModel
            in
            ( { model | page = Auth subModel }, Cmd.map GotAuthMsg subMsg )

        -- combining the msg and the model.page allow us to filter out
        -- messages coming from the wrong page
        ( _, _ ) ->
            ( model, Cmd.none )


loadRoute : Maybe Route.Route -> Model -> ( Model, Cmd Msg )
loadRoute maybeRoute model =
    -- get session from the current page model
    let
       session = toSession model.page
    in
    
    case maybeRoute of
        -- no matching route so 404 page is selected
        -- could we create an Error module which will manage this kind of pages
        -- see package elm app
        Nothing ->
            ( { model | page = NotFound session }, Cmd.none )

        Just Route.Home ->
            let
                ( subModel, subMsg ) =
                    Home.init session
            in
            ( { model | page = Home subModel }, Cmd.map GotHomeMsg subMsg )

        Just (Route.Auth Nothing) ->
            let
                ( subModel, subMsg ) =
                    Auth.init session
            in
            ( { model | page = Auth subModel }, Cmd.map GotAuthMsg subMsg )

        Just (Route.Auth _) ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Browser.Document Msg
view model =
    case model.page of
        Home home ->
            Page.view GotHomeMsg (Home.view home)

        Auth authModel ->
            Page.view GotAuthMsg (Auth.view authModel)

        NotFound _ ->
            { title = "Not Found"
            , body =
                [ a [ href "/" ] [ img [ Asset.src Asset.logo, class "center db pt2" ] [] ]
                , h1 [ class "tc" ] [ text "page not found" ]
                ]
            }


toSession : Page -> Session.Session
toSession page =
    case page of
       NotFound session -> session

       Home m -> Home.toSession m

       Auth m -> Auth.toSession m