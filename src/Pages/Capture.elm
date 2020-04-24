module Pages.Capture exposing (Model, Msg(..), SortCaptures(..), init, sortCaptures, subscriptions, toSession, update, view)

import Asset
import Browser.Dom as Dom
import Capture exposing (..)
import Element exposing (..)
import Element.Background as EltBackground
import Element.Events exposing (onClick)
import Element.Font exposing (..)
import Element.Input as EltInput
import Endpoint
import Html.Attributes exposing (id)
import Http
import Page
import Route
import Session exposing (..)
import Task
import Time
import Timer exposing (..)
import UI.Nav
import UI.UI as UI



-- Model


type alias Model =
    { session : Session
    , nav : UI.Nav.State
    , captures : List Capture
    , sortCaptures : SortCaptures
    , pageStatus : PageStatus
    , newCapture : Capture
    , timer : Clock
    , error : String
    }


type SortCaptures
    = Status Capture.CaptureStatus


type PageStatus
    = Loading
    | Ready


type alias Clock =
    { zone : Time.Zone
    , posix : Time.Posix
    }


initModel : Session -> Model
initModel session =
    { session = session
    , nav = UI.Nav.init
    , captures = []
    , sortCaptures = Status InProgress
    , pageStatus = Loading
    , newCapture = initCapture
    , timer =
        { zone = Time.utc
        , posix = Time.millisToPosix 0
        }
    , error = ""
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( initModel session
    , Cmd.batch
        [ apiGetCaptures (token session)
        , Task.perform AdjustTimeZone Time.here
        , Task.attempt (\_ -> None) (Dom.focus "capture-text")
        ]
    )



-- Update


type Msg
    = GotSession Session
    | GotCaptures (Result Http.Error (List Capture))
    | CaptureSaved (Result Http.Error Capture)
    | UpdateNewCapture String
    | AddCapture
    | StartTimer Int
    | StopTimer Int Int
    | TimerUpdated CaptureStatus (Result Http.Error Timer)
    | None
    | AdjustTimeZone Time.Zone
    | Tick Time.Posix
    | ToggleCompleted Capture Bool
    | SortBy SortCaptures
    | SetNavState UI.Nav.State


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )

        AdjustTimeZone zone ->
            let
                timer =
                    model.timer

                newTimer =
                    { timer | zone = zone }
            in
            ( { model | timer = newTimer }, Cmd.none )

        Tick posix ->
            let
                timer =
                    model.timer

                newTimer =
                    { timer | posix = posix }
            in
            ( { model | timer = newTimer }, Cmd.none )

        GotSession session ->
            if Session.isGuest session then
                ( { model | session = session }
                , Route.replaceUrl (Session.navKey model.session) Route.Login
                )

            else
                ( { model | session = session }
                , Route.replaceUrl (Session.navKey model.session) Route.Capture
                )

        GotCaptures result ->
            case result of
                Ok captures ->
                    ( { model | captures = captures, pageStatus = Ready, error = "" }, Cmd.none )

                Err httpError ->
                    case httpError of
                        Http.BadStatus 401 ->
                            -- redirect to login page if not logged in anymore
                            ( { model | error = "Access not authorised" }
                            , Cmd.batch
                                [ Session.logout
                                , Route.replaceUrl (Session.navKey model.session) Route.Login
                                ]
                            )

                        Http.BadStatus 404 ->
                            ( { model | error = "User information can't be retrieved" }, Cmd.none )

                        _ ->
                            ( { model | error = "Error while getting the captures" }, Cmd.none )

        CaptureSaved result ->
            case result of
                Ok _ ->
                    ( { model | error = "" }, apiGetCaptures (token model.session) )

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

        UpdateNewCapture text ->
            let
                capture =
                    model.newCapture

                newCapture =
                    { capture | text = text }
            in
            ( { model | newCapture = newCapture }, Cmd.none )

        -- save capture in database
        AddCapture ->
            ( { model | newCapture = initCapture }, apiSaveCapture (token model.session) model.newCapture )

        StartTimer idCapture ->
            let
                captures =
                    updateCapture Disabled idCapture model.captures
            in
            ( { model | captures = captures, sortCaptures = Status InProgress }, startTimer (token model.session) idCapture )

        StopTimer idTimer idCapture ->
            ( model, stopTimer (token model.session) idTimer idCapture )

        TimerUpdated _ result ->
            case result of
                Ok _ ->
                    ( { model | error = "" }, apiGetCaptures (token model.session) )

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
                            ( { model | error = "create timer endpoint not found" }, Cmd.none )

                        _ ->
                            ( { model | error = "Error while starting the timer" }, Cmd.none )

        ToggleCompleted capture _ ->
            case capture.status of
                Completed ->
                    ( model
                    , apiUpdateCapture (token model.session) { capture | status = ToDo }
                    )

                _ ->
                    ( model
                    , apiUpdateCapture (token model.session) { capture | status = Completed }
                    )

        SortBy status ->
            ( { model | sortCaptures = status }, Cmd.none )

        SetNavState navState ->
            ( { model | nav = navState }, Cmd.none )



-- View


view : Model -> Page.PageStructure Msg
view model =
    { title = "Capture"
    , content =
        [ layout
            [ family
                [ typeface "Montserrat"
                , sansSerif
                ]
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
                    , el [ width fill, center, bold ] (text "Items")
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
                , if String.isEmpty model.error then
                    column [ width fill, height fill, spacing 50 ]
                        [ column [ centerX, spacing 10 ]
                            [ EltInput.text [ EltInput.focusedOnLoad, htmlAttribute <| id "capture-text" ]
                                { onChange = UpdateNewCapture
                                , text = model.newCapture.text
                                , placeholder = Just (EltInput.placeholder [] (text "capture text"))
                                , label = EltInput.labelHidden "capture text"
                                }
                            , EltInput.button
                                UI.mintButtonAttrs
                                { onPress =
                                    if String.isEmpty model.newCapture.text then
                                        Nothing

                                    else
                                        Just AddCapture
                                , label = text "Add Capture"
                                }
                            ]
                        , if model.pageStatus == Ready then
                            column [ width (fill |> maximum 1000), centerX ]
                                [ showSortingOptions model.sortCaptures
                                , column
                                    [ width fill
                                    , spacing 30
                                    , padding 30
                                    ]
                                  <|
                                    List.map
                                        (\capture -> showCapture model.timer capture)
                                        (sortCaptures model.sortCaptures model.captures)
                                ]

                          else
                            column [ centerX, spacing 20 ]
                                [ text "Loading captures"
                                , el [ centerX ] <| html UI.loaderHtml
                                ]
                        ]

                  else
                    el [ color (rgb255 255 65 54) ] (text model.error)
                ]
        ]
    }


toSession : Model -> Session
toSession model =
    model.session


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 1000 Tick
        , Session.changeSession GotSession (Session.navKey model.session)
        ]



-- captures


sortCaptures : SortCaptures -> List Capture -> List Capture
sortCaptures (Status sortBy) =
    List.sortWith (compareCaptures sortBy)


startTimer : String -> Int -> Cmd Msg
startTimer token idCapture =
    Http.request
        { method = "POST"
        , headers = [ Http.header "authorization" ("Bearer " ++ token) ]
        , url = Endpoint.toString (Endpoint.startTimer idCapture)
        , body = Http.emptyBody
        , expect = Http.expectJson (TimerUpdated InProgress) timerDataDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


stopTimer : String -> Int -> Int -> Cmd Msg
stopTimer token idTimer idCapture =
    Http.request
        { method = "PUT"
        , headers = [ Http.header "authorization" ("Bearer " ++ token) ]
        , url = Endpoint.toString (Endpoint.stopTimer idTimer idCapture)
        , body = Http.jsonBody <| actionTimerEncode "stop"
        , expect = Http.expectJson (TimerUpdated ToDo) timerDataDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


showSortingOptions : SortCaptures -> Element Msg
showSortingOptions (Status s) =
    row [ spacing 20, width fill ]
        [ el [ width fill, Element.alignRight ] none
        , el [ width fill, Element.alignRight ] none
        , el
            [ width fill, Element.alignRight, center ]
            (EltInput.button
                [ centerX, center, padding 15, width (fill |> maximum 150) ]
                { onPress = Just <| SortBy (nextSortCaptures (Status s)), label = text <| captureStatusToString s ++ " ↕" }
            )
        ]


nextSortCaptures : SortCaptures -> SortCaptures
nextSortCaptures (Status s) =
    case s of
        InProgress ->
            Status Completed

        Completed ->
            Status ToDo

        ToDo ->
            Status InProgress

        _ ->
            Status s


showCapture : Clock -> Capture -> Element Msg
showCapture clock capture =
    let
        completed =
            capture.status == Completed
    in
    column [ width fill, spacing 10, centerX ]
        [ row [ spacing 20, width fill ]
            [ EltInput.checkbox [ width fill, color UI.darkGrey ]
                { onChange = ToggleCompleted capture
                , icon = EltInput.defaultCheckbox
                , checked = completed
                , label =
                    if completed then
                        EltInput.labelRight [ strike, width fill ] (paragraph [] [ text capture.text ])

                    else
                        EltInput.labelRight [ width fill ] (paragraph [] [ text capture.text ])
                }
            , showTime capture clock
            , case capture.status of
                ToDo ->
                    showTimerButton UI.startButtonAttrs "start" (StartTimer capture.idCapture)

                InProgress ->
                    let
                        timer =
                            getCurrentTimer capture.timers
                    in
                    case timer of
                        Nothing ->
                            showTimerButton [] "Error Timer" None

                        Just t ->
                            showTimerButton UI.stopButtonAttrs "stop" (StopTimer t.idTimer capture.idCapture)

                Disabled ->
                    showTimerButton UI.completedButtonAttrs "..." None

                Completed ->
                    showTimerButton UI.completedButtonAttrs "completed" None

                Error e ->
                    showTimerButton [] e None
            ]
        , el [ padding 5 ] (text <| String.join ", " capture.tags)
        , link
            [ color UI.teal ]
            { url = Route.routeToString (Route.CaptureEdit capture.idCapture)
            , label = text "edit"
            }
        , el [ width (fill |> maximum 1000), centerX, EltBackground.color UI.lightGrey, height (px 1) ] none
        ]


showTimerButton : List (Attribute Msg) -> String -> Msg -> Element Msg
showTimerButton attrs textButton msg =
    el [ width fill, Element.alignRight ]
        (EltInput.button ([ center, width (fill |> maximum 150) ] ++ attrs) { onPress = Just msg, label = text textButton })


showTime : Capture -> Clock -> Element Msg
showTime capture clock =
    case capture.status of
        InProgress ->
            let
                maybeStartedAtMillis =
                    Maybe.map (\t -> Time.posixToMillis t.startedAt) (getCurrentTimer capture.timers)

                nowToMillis =
                    Time.posixToMillis clock.posix

                maybeNowMillis =
                    Maybe.map ((-) nowToMillis) maybeStartedAtMillis

                maybePreviousTimersMillis =
                    getPreviousTimer capture.timers
                        |> Maybe.andThen timersToMillis

                allTimes =
                    Maybe.map2 (+) maybeNowMillis maybePreviousTimersMillis

                ( hour, minute, second ) =
                    case allTimes of
                        Nothing ->
                            ( "", "", "" )

                        Just t ->
                            millisToHMS t
            in
            link [ center, bold, width fill, padding 30, color UI.teal ]
                { url = Route.routeToString (Route.CaptureTimers capture.idCapture)
                , label = text (hour ++ ":" ++ minute ++ ":" ++ second)
                }

        _ ->
            let
                ( hour, minute, second ) =
                    case timersToMillis capture.timers of
                        Nothing ->
                            ( "", "", "" )

                        Just t ->
                            millisToHMS t
            in
            link [ center, bold, width fill, padding 30, color UI.teal ]
                { url = Route.routeToString (Route.CaptureTimers capture.idCapture)
                , label = text (hour ++ ":" ++ minute ++ ":" ++ second)
                }


apiGetCaptures : String -> Cmd Msg
apiGetCaptures token =
    Http.request
        { method = "GET"
        , headers = [ Http.header "authorization" ("Bearer " ++ token) ]
        , url = Endpoint.toString Endpoint.captures
        , body = Http.emptyBody
        , expect = Http.expectJson GotCaptures capturesDataDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


apiSaveCapture : String -> Capture -> Cmd Msg
apiSaveCapture token capture =
    Http.request
        { method = "POST"
        , headers = [ Http.header "authorization" ("Bearer " ++ token) ]
        , url = Endpoint.toString Endpoint.captures
        , body = Http.jsonBody <| captureEncode capture
        , expect = Http.expectJson CaptureSaved savedCaptureDecoder
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


updateCapture : CaptureStatus -> Int -> List Capture -> List Capture
updateCapture status idCapture listCaptures =
    List.map
        (\c ->
            if idCapture == c.idCapture then
                { c | status = status }

            else
                c
        )
        listCaptures
