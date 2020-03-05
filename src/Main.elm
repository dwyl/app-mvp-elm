module Main exposing (main)

import Asset
import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Page
import Pages.Auth as Auth
import Pages.Home as Home
import Pages.Session as PagesSession
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


type Model
    = Home Home.Model
    | Auth Auth.Model
    | Session PagesSession.Model
    | NotFound Session.Session



-- flags will contain the session from local storage
-- init look at the url, parse it and load the Page (ie Home, Auth...)


init : Maybe String -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        session =
            case flags of
                Nothing ->
                    Session.Guest navKey

                Just str ->
                    Session.decode navKey str
    in
    loadRoute (Route.fromUrl url) (NotFound session)



-- UPDATE


type Msg
    = UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | GotHomeMsg Home.Msg
    | GotAuthMsg Auth.Msg
    | GotPagesSessionMsg PagesSession.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( LinkClicked link, _ ) ->
            case link of
                Browser.Internal urlRequested ->
                    ( model, Nav.pushUrl (toNavKey model) (Url.toString urlRequested) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( UrlChanged url, _ ) ->
            loadRoute (Route.fromUrl url) model

        ( GotHomeMsg homeMsg, Home homeModel ) ->
            let
                ( subModel, subMsg ) =
                    Home.update homeMsg homeModel
            in
            ( Home subModel, Cmd.map GotHomeMsg subMsg )

        ( GotAuthMsg authMsg, Auth authModel ) ->
            let
                ( subModel, subMsg ) =
                    Auth.update authMsg authModel
            in
            ( Auth subModel, Cmd.map GotAuthMsg subMsg )

        ( GotPagesSessionMsg sessionMsg, Session sessionModel ) ->
            let
                ( subModel, subMsg ) =
                    PagesSession.update sessionMsg sessionModel
            in
            ( Session subModel, Cmd.map GotPagesSessionMsg subMsg )

        -- combining the msg and the model.page allow us to filter out
        -- messages coming from the wrong page
        ( _, _ ) ->
            ( model, Cmd.none )


loadRoute : Maybe Route.Route -> Model -> ( Model, Cmd Msg )
loadRoute maybeRoute model =
    -- get session from the current page model
    let
        session =
            toSession model
    in
    case maybeRoute of
        -- no matching route so 404 page is selected
        -- could we create an Error module which will manage this kind of pages
        -- see package elm app
        Nothing ->
            ( NotFound session, Cmd.none )

        Just Route.Home ->
            let
                ( subModel, subMsg ) =
                    Home.init session
            in
            ( Home subModel, Cmd.map GotHomeMsg subMsg )

        Just (Route.Auth Nothing) ->
            let
                ( subModel, subMsg ) =
                    Auth.init session
            in
            ( Auth subModel, Cmd.map GotAuthMsg subMsg )

        Just (Route.Auth (Just jwt)) ->
            let
                ( subModel, subMsg ) =
                    PagesSession.init session jwt
            in
            ( Session subModel, Cmd.map GotPagesSessionMsg subMsg )

        Just Route.Logout ->
            ( model, Session.logout )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Home home ->
            Sub.map GotHomeMsg (Home.subscriptions home)

        Auth authModel ->
            Sub.map GotAuthMsg (Auth.subscriptions authModel)

        Session sessionModel ->
            Sub.map GotPagesSessionMsg (PagesSession.subscriptions sessionModel)

        NotFound _ ->
            Sub.none


view : Model -> Browser.Document Msg
view model =
    case model of
        Home home ->
            Page.view GotHomeMsg (Home.view home)

        Auth authModel ->
            Page.view GotAuthMsg (Auth.view authModel)

        Session sessionModel ->
            Page.view GotPagesSessionMsg (PagesSession.view sessionModel)

        NotFound _ ->
            { title = "Not Found"
            , body =
                [ a [ href "/" ] [ img [ Asset.src Asset.logo, class "center db pt2" ] [] ]
                , h1 [ class "tc" ] [ text "page not found" ]
                ]
            }


toSession : Model -> Session.Session
toSession page =
    case page of
        NotFound session ->
            session

        Home m ->
            Home.toSession m

        Auth m ->
            Auth.toSession m

        Session m ->
            PagesSession.toSession m


toNavKey : Model -> Nav.Key
toNavKey model =
    Session.navKey (toSession model)
