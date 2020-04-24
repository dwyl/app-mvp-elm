module Pages.CaptureEdit exposing (Model, Msg(..), init, subscriptions, toSession, update, view)

import Asset exposing (..)
import Capture exposing (..)
import Element exposing (..)
import Element.Background as EltBackground
import Element.Events exposing (onClick)
import Element.Font exposing (..)
import Element.Input as EltInput
import Endpoint
import Http
import Page
import Route
import Session exposing (..)
import UI.Nav
import UI.UI as UI



-- Model


type alias Model =
    { session : Session
    , capture : Capture
    , error : String
    , nav : UI.Nav.State
    }


init : Session -> Int -> ( Model, Cmd Msg )
init session idCapture =
    ( Model session initCapture "" UI.Nav.init
    , getCapture (token session) idCapture
    )



-- Update


type Msg
    = GotSession Session
    | GotCapture (Result Http.Error Capture)
    | SetNavState UI.Nav.State
    | UpdateCaptureText String
    | UpdateCaptureTags String
    | SaveCapture
    | CaptureSaved (Result Http.Error Capture)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSession session ->
            if Session.isGuest session then
                ( { model | session = session }
                , Route.replaceUrl (Session.navKey session) Route.Login
                )

            else
                ( { model | session = session }
                , Route.replaceUrl (Session.navKey session) Route.Capture
                )

        GotCapture result ->
            case result of
                Ok capture ->
                    ( { model | capture = capture, error = "" }, Cmd.none )

                Err httpError ->
                    case httpError of
                        Http.BadStatus 401 ->
                            ( { model | error = "Access not authorised" }
                            , Cmd.batch
                                [ Session.logout
                                , Route.replaceUrl (Session.navKey model.session) Route.Login
                                ]
                            )

                        Http.BadStatus 404 ->
                            ( { model | error = "Capture information can't be retrieved" }, Cmd.none )

                        _ ->
                            ( { model | error = "Error while getting the captures" }, Cmd.none )

        SetNavState navState ->
            ( { model | nav = navState }, Cmd.none )

        UpdateCaptureText text ->
            let
                capture =
                    model.capture

                updatedCapture =
                    { capture | text = text }
            in
            ( { model | capture = updatedCapture }, Cmd.none )

        UpdateCaptureTags tags ->
            let
                capture =
                    model.capture

                tagList =
                    List.map String.trim (String.split "," tags)

                updatedCapture =
                    { capture | tags = tagList }
            in
            ( { model | capture = updatedCapture }, Cmd.none )

        SaveCapture ->
            ( model, apiUpdateCapture (token model.session) model.capture )

        CaptureSaved result ->
            case result of
                Ok _ ->
                    ( { model | error = "" }
                    , Route.replaceUrl (Session.navKey model.session) Route.Capture
                    )

                Err httpError ->
                    case httpError of
                        Http.BadStatus 401 ->
                            ( { model | error = "Access not authorised" }
                            , Cmd.batch
                                [ Session.logout
                                , Route.replaceUrl (Session.navKey model.session) Route.Login
                                ]
                            )

                        Http.BadStatus 404 ->
                            ( { model | error = "create capture endpoint not found" }, Cmd.none )

                        _ ->
                            ( { model | error = "Error while creating the capture" }, Cmd.none )



-- Request to API


getCapture : String -> Int -> Cmd Msg
getCapture token idCapture =
    Http.request
        { method = "GET"
        , headers = [ Http.header "authorization" ("Bearer " ++ token) ]
        , url = Endpoint.toString (Endpoint.capture idCapture)
        , body = Http.emptyBody
        , expect = Http.expectJson GotCapture captureDataDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


apiUpdateCapture : String -> Capture -> Cmd Msg
apiUpdateCapture token capture =
    Http.request
        { method = "PUT"
        , headers = [ Http.header "authorization" ("Bearer " ++ token) ]
        , url = Endpoint.toString <| Endpoint.capture capture.idCapture
        , body = Http.jsonBody <| captureEncode capture
        , expect = Http.expectJson CaptureSaved savedCaptureDecoder
        , timeout = Nothing
        , tracker = Nothing
        }



-- View


view : Model -> Page.PageStructure Msg
view model =
    { title = "Edit Capture"
    , content =
        [ layout
            [ family [ typeface "Montserrat", sansSerif ]
            , if UI.Nav.isOpen model.nav then
                inFront <| UI.Nav.view (UI.Nav.config { toMsg = SetNavState, session = model.session }) model.nav

              else
                inFront none
            ]
          <|
            column
                [ width fill
                , height fill
                , spacing 30
                , if UI.Nav.isOpen model.nav then
                    alpha 0.5

                  else
                    alpha 1
                ]
                [ row [ width fill, padding 20, EltBackground.color UI.lightGrey ]
                    [ el [ width fill ]
                        (el [ width (px 50) ]
                            (link []
                                { url = Route.routeToString Route.Capture
                                , label =
                                    image [ centerX ]
                                        { src =
                                            if String.isEmpty (Session.avatar model.session) then
                                                Asset.imagePath Asset.logo

                                            else
                                                Session.avatar model.session
                                        , description = "User Avatar"
                                        }
                                }
                            )
                        )
                    , el [ width fill, center, bold ] (text "Item")
                    , el
                        [ width fill
                        , Element.Font.alignRight
                        ]
                        (column
                            [ width (fill |> maximum 40)
                            , Element.alignRight
                            , spacing 10
                            , pointer
                            , onClick (SetNavState <| UI.Nav.toggleNav model.nav)
                            ]
                            [ el [ width fill, centerX, EltBackground.color UI.mint, height (px 3) ] none
                            , el [ width fill, centerX, EltBackground.color UI.mint, height (px 3) ] none
                            , el [ width fill, centerX, EltBackground.color UI.mint, height (px 3) ] none
                            ]
                        )
                    ]
                , column
                    [ width (fill |> maximum 1000), centerX ]
                    [ showEditForm model.capture
                    ]
                ]
        ]
    }


showEditForm : Capture -> Element Msg
showEditForm capture =
    column [ width fill, spacing 30 ]
        [ EltInput.text []
            { onChange = UpdateCaptureText
            , placeholder = Nothing
            , text = capture.text
            , label = EltInput.labelLeft [ centerY ] (text "text: ")
            }
        , EltInput.text []
            { onChange = UpdateCaptureTags
            , placeholder = Nothing
            , text = String.join ", " capture.tags
            , label = EltInput.labelLeft [ centerY ] (text "tags: ")
            }
        , EltInput.button
            UI.mintButtonAttrs
            { onPress = Just SaveCapture
            , label = text "Save"
            }
        ]


toSession : Model -> Session
toSession model =
    model.session


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changeSession GotSession (Session.navKey model.session)
