module Pages.Capture exposing (Model, Msg(..), init, subscriptions, toSession, update, view)

import Asset
import Capture exposing (..)
import Endpoint
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Page
import Route
import Session exposing (..)
import Timer exposing (..)



-- Model


type alias Model =
    { session : Session
    , captures : List Capture
    , newCapture : Capture
    , error : String
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( Model session [] initCapture "", getCaptures (token session) )



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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )

        GotSession session ->
            ( { model | session = session }
            , Route.replaceUrl (Session.navKey model.session) Route.Home
            )

        GotCaptures result ->
            case result of
                Ok captures ->
                    let
                        _ =
                            Debug.log "captures" captures
                    in
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
                    ( { model | error = "" }, getCaptures (token model.session) )

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
            ( { model | newCapture = initCapture }, saveCapture (token model.session) model.newCapture )

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
                    ( { model | error = "" }, getCaptures (token model.session) )

                Err httpError ->
                    case httpError of
                        Http.BadStatus 401 ->
                            ( { model | error = "Access not authorised" }, Cmd.none )

                        Http.BadStatus 404 ->
                            ( { model | error = "create timer endpoint not found" }, Cmd.none )

                        _ ->
                            ( { model | error = "Error while starting the timer" }, Cmd.none )



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

            Session.Session _ _ ->
                div []
                    [ if String.isEmpty model.error then
                        div []
                            [ div [ class "w-60 center tc" ]
                                [ input [ class "w-80 mr2", value model.newCapture.text, onInput UpdateNewCapture ] []
                                , button [ class "pointer", onClick AddCapture ] [ text "Add Capture" ]
                                ]
                            , div [ class "w-50 center" ] <| List.map (\capture -> showCapture capture) model.captures
                            ]

                      else
                        p [ class "red tc" ] [ text model.error ]
                    ]
        ]
    }


toSession : Model -> Session
toSession model =
    model.session


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changeSession GotSession (Session.navKey model.session)



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


showCapture : Capture -> Html Msg
showCapture capture =
    div [ class "pa2" ]
        [ label
            [ class "dib pa2" ]
            [ input [ type_ "checkbox", checked False, disabled False, class "mr2" ] []
            , text <| capture.text
            ]
        , case capture.status of
            ToDo ->
                showTimerButton "start" (StartTimer capture.idCapture)

            InProgress ->
                let
                    timer =
                        getTimer capture
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


showTimerButton : String -> Msg -> Html Msg
showTimerButton textButton msg =
    button
        [ disabled False
        , class "fr"
        , classList [ ( "pointer", True ) ]
        , onClick msg
        ]
        [ text textButton ]


getCaptures : String -> Cmd Msg
getCaptures token =
    Http.request
        { method = "GET"
        , headers = [ Http.header "authorization" ("Bearer " ++ token) ]
        , url = Endpoint.toString Endpoint.captures
        , body = Http.emptyBody
        , expect = Http.expectJson GotCaptures capturesDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


saveCapture : String -> Capture -> Cmd Msg
saveCapture token capture =
    Http.request
        { method = "POST"
        , headers = [ Http.header "authorization" ("Bearer " ++ token) ]
        , url = Endpoint.toString Endpoint.captures
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
