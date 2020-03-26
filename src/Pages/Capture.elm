module Pages.Capture exposing (Model, Msg(..), init, subscriptions, toSession, update, view)

import Asset
import Capture exposing (..)
import Element exposing (..)
import Element.Font exposing (..)
import Element.Input as EltInput
import Endpoint
import Http
import Page
import Route
import Session exposing (..)
import Task
import Time
import Timer exposing (..)
import UI



-- Model


type alias Model =
    { session : Session
    , captures : List Capture
    , newCapture : Capture
    , timer : Clock
    , error : String
    }


type alias Clock =
    { zone : Time.Zone
    , posix : Time.Posix
    }


initModel : Session -> Model
initModel session =
    { session = session
    , captures = []
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
            ( { model | session = session }
            , Route.replaceUrl (Session.navKey model.session) Route.Home
            )

        GotCaptures result ->
            case result of
                Ok captures ->
                    ( { model | captures = captures, error = "" }, Cmd.none )

                Err httpError ->
                    case httpError of
                        Http.BadStatus 401 ->
                            ( { model | error = "Access not authorised" }, Cmd.none )

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
                            ( { model | error = "Access not authorised" }, Cmd.none )

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
            ( { model | captures = captures }, startTimer (token model.session) idCapture )

        StopTimer idTimer idCapture ->
            ( model, stopTimer (token model.session) idTimer idCapture )

        TimerUpdated _ result ->
            case result of
                Ok _ ->
                    ( { model | error = "" }, apiGetCaptures (token model.session) )

                Err httpError ->
                    case httpError of
                        Http.BadStatus 401 ->
                            ( { model | error = "Access not authorised" }, Cmd.none )

                        Http.BadStatus 404 ->
                            ( { model | error = "create timer endpoint not found" }, Cmd.none )

                        _ ->
                            ( { model | error = "Error while starting the timer" }, Cmd.none )

        ToggleCompleted capture _ ->
            let
                captureStatus =
                    if capture.status == Completed then
                        ToDo

                    else
                        Completed

                updatedCapture =
                    { capture | status = captureStatus }
            in
            ( model, apiUpdateCapture (token model.session) updatedCapture )



-- View


view : Model -> Page.PageStructure Msg
view model =
    { title = "Capture"
    , content =
        [ layout [] <|
            column [ Element.width Element.fill ]
                [ column [ centerX ]
                    [ link []
                        { url = Route.routeToString Route.Home
                        , label = image [ centerX ] { src = Asset.imagePath Asset.logo, description = "DWYL Logo" }
                        }
                    , el [] (text "DWYL Application")
                    ]
                , case model.session of
                    Session.Guest _ ->
                        link [ centerX ]
                            { url = Route.routeToString Route.Home
                            , label = text "Not logged in yet!"
                            }

                    Session.Session _ _ ->
                        if String.isEmpty model.error then
                            column [ width fill ]
                                [ column [ centerX, spacing 10, padding 30 ]
                                    [ EltInput.text []
                                        { onChange = UpdateNewCapture
                                        , text = model.newCapture.text
                                        , placeholder = Nothing
                                        , label = EltInput.labelHidden "capture text"
                                        }
                                    , EltInput.button
                                        UI.buttonAttrs
                                        { onPress = Just AddCapture, label = text "Add Capture" }
                                    ]
                                , column
                                    [ width fill
                                    , spacing 30
                                    , padding 30
                                    ]
                                  <|
                                    List.map (\capture -> showCapture model.timer capture) model.captures
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


showCapture : Clock -> Capture -> Element Msg
showCapture clock capture =
    let
        completed =
            capture.status == Completed
    in
    column
        [ centerX
        , spacing 10
        , width (fill |> maximum 500)
        ]
        [ row [ spacing 30, width fill ]
            [ EltInput.checkbox [ Element.alignLeft ]
                { onChange = ToggleCompleted capture
                , icon = EltInput.defaultCheckbox
                , checked = completed
                , label =
                    if completed then
                        EltInput.labelRight [ strike ] (text capture.text)

                    else
                        EltInput.labelRight [] (text capture.text)
                }
            , case capture.status of
                ToDo ->
                    showTimerButton "start" (StartTimer capture.idCapture)

                InProgress ->
                    let
                        timer =
                            getCurrentTimer capture.timers
                    in
                    case timer of
                        Nothing ->
                            showTimerButton "Error Timer" None

                        Just t ->
                            showTimerButton "stop" (StopTimer t.idTimer capture.idCapture)

                Disabled ->
                    showTimerButton "..." None

                Completed ->
                    showTimerButton "completed" None

                Error e ->
                    showTimerButton e None
            ]
        , showTime capture clock
        ]


showTimerButton : String -> Msg -> Element Msg
showTimerButton textButton msg =
    EltInput.button (Element.alignRight :: UI.buttonAttrs) { onPress = Just msg, label = text textButton }


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
            link [ centerX ]
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
            link [ centerX ]
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
