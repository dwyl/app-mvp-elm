module Main exposing (main)

import Asset
import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Page
import Pages.Auth as Auth
import Pages.Capture as Capture
import Pages.CaptureEdit as CaptureEdit
import Pages.CaptureTimers as CaptureTimers
import Pages.Login as Login
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
    = Auth Auth.Model
    | Session PagesSession.Model
    | NotFound Session.Session
    | Logout Session.Session
    | Login Session.Session
    | Capture Capture.Model
    | CaptureEdit CaptureEdit.Model
    | CaptureTimers CaptureTimers.Model



-- flags will contain the session from local storage
-- init look at the url, parse it and load the Page (ie Auth, Capture...)


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
    | GotAuthMsg Auth.Msg
    | GotPagesSessionMsg PagesSession.Msg
    | GotCaptureMsg Capture.Msg
    | GotCaptureEditMsg CaptureEdit.Msg
    | GotCaptureTimersMsg CaptureTimers.Msg
    | GotLoginMsg Login.Msg


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

        ( GotCaptureMsg captureMsg, Capture captureModel ) ->
            let
                ( subModel, subMsg ) =
                    Capture.update captureMsg captureModel
            in
            ( Capture subModel, Cmd.map GotCaptureMsg subMsg )

        ( GotCaptureEditMsg captureEditMsg, CaptureEdit captureEditModel ) ->
            let
                ( subModel, subMsg ) =
                    CaptureEdit.update captureEditMsg captureEditModel
            in
            ( CaptureEdit subModel, Cmd.map GotCaptureEditMsg subMsg )

        ( GotCaptureTimersMsg captureTimersMsg, CaptureTimers captureTimersModel ) ->
            let
                ( subModel, subMsg ) =
                    CaptureTimers.update captureTimersMsg captureTimersModel
            in
            ( CaptureTimers subModel, Cmd.map GotCaptureTimersMsg subMsg )

        ( GotLoginMsg loginMsg, Login loginModel ) ->
            let
                ( subModel, subMsg ) =
                    Login.update loginMsg loginModel
            in
            ( Login subModel, Cmd.map GotLoginMsg subMsg )

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

        privateRoute =
            Maybe.withDefault False <| Maybe.map Route.isPrivate maybeRoute
    in
    -- redirect to login page if user attempt to access private page as guest
    if Session.isGuest session && privateRoute then
        ( model
        , Route.replaceUrl (Session.navKey session) Route.Login
        )

    else
        case maybeRoute of
            Nothing ->
                ( NotFound session, Cmd.none )

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
                ( Logout session
                , Cmd.batch
                    [ Session.logout
                    , Route.replaceUrl (Session.navKey session) Route.Login
                    ]
                )

            Just Route.Login ->
                let
                    ( subModel, subMsg ) =
                        Login.init session
                in
                ( Login subModel, Cmd.map GotLoginMsg subMsg )

            Just Route.Capture ->
                let
                    ( subModel, subMsg ) =
                        Capture.init session
                in
                ( Capture subModel, Cmd.map GotCaptureMsg subMsg )

            Just (Route.CaptureTimers idCapture) ->
                let
                    ( subModel, subMsg ) =
                        CaptureTimers.init session idCapture
                in
                ( CaptureTimers subModel, Cmd.map GotCaptureTimersMsg subMsg )

            Just (Route.CaptureEdit idCapture) ->
                let
                    ( subModel, subMsg ) =
                        CaptureEdit.init session idCapture
                in
                ( CaptureEdit subModel, Cmd.map GotCaptureEditMsg subMsg )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Auth authModel ->
            Sub.map GotAuthMsg (Auth.subscriptions authModel)

        Session sessionModel ->
            Sub.map GotPagesSessionMsg (PagesSession.subscriptions sessionModel)

        NotFound _ ->
            Sub.none

        Logout _ ->
            Sub.none

        Capture captureModel ->
            Sub.map GotCaptureMsg (Capture.subscriptions captureModel)

        CaptureEdit captureEditModel ->
            Sub.map GotCaptureEditMsg (CaptureEdit.subscriptions captureEditModel)

        CaptureTimers captureTimersModel ->
            Sub.map GotCaptureTimersMsg (CaptureTimers.subscriptions captureTimersModel)

        Login loginModel ->
            Sub.map GotLoginMsg (Login.subscriptions loginModel)


view : Model -> Browser.Document Msg
view model =
    case model of
        Auth authModel ->
            Page.view GotAuthMsg (Auth.view authModel)

        Session sessionModel ->
            Page.view GotPagesSessionMsg (PagesSession.view sessionModel)

        NotFound _ ->
            { title = "Not Found"
            , body =
                [ a [ Route.href Route.Capture ] [ img [ Asset.src Asset.logo, class "center db pt2" ] [] ]
                , h1 [ class "tc" ] [ text "page not found" ]
                ]
            }

        Logout _ ->
            { title = "Logout"
            , body =
                [ a [ Route.href Route.Capture ] [ img [ Asset.src Asset.logo, class "center db pt2" ] [] ]
                , h1 [ class "tc" ] [ text "Logout" ]
                ]
            }

        Login loginModel ->
            Page.view GotLoginMsg (Login.view loginModel)

        Capture captureModel ->
            Page.view GotCaptureMsg (Capture.view captureModel)

        CaptureEdit captureEditModel ->
            Page.view GotCaptureEditMsg (CaptureEdit.view captureEditModel)

        CaptureTimers captureTimersModel ->
            Page.view GotCaptureTimersMsg (CaptureTimers.view captureTimersModel)


toSession : Model -> Session.Session
toSession page =
    case page of
        NotFound session ->
            session

        Auth m ->
            Auth.toSession m

        Session m ->
            PagesSession.toSession m

        Logout session ->
            session

        Login m ->
            Login.toSession m

        Capture m ->
            Capture.toSession m

        CaptureEdit m ->
            CaptureEdit.toSession m

        CaptureTimers m ->
            CaptureTimers.toSession m


toNavKey : Model -> Nav.Key
toNavKey model =
    Session.navKey (toSession model)
