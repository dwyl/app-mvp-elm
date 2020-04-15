module Pages.CaptureTimers exposing (Model, Msg(..), init, subscriptions, toSession, update, view)

import Asset
import Capture exposing (..)
import Element exposing (..)
import Element.Background as EltBackground
import Element.Events exposing (onClick)
import Element.Font exposing (..)
import Endpoint
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
    { capture : Capture
    , nav : UI.Nav.State
    , session : Session
    , timeZone : Time.Zone
    , error : String
    }


initModel : Session -> Model
initModel session =
    { capture = initCapture
    , nav = UI.Nav.init
    , session = session
    , timeZone = Time.utc
    , error = ""
    }


init : Session -> Int -> ( Model, Cmd Msg )
init session idCapture =
    ( initModel session
    , Cmd.batch
        [ Task.perform AdjustTimeZone Time.here
        , getCapture (token session) idCapture
        ]
    )



-- Update


type Msg
    = GotSession Session
    | GotCapture (Result Http.Error Capture)
    | AdjustTimeZone Time.Zone
    | SetNavState UI.Nav.State


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AdjustTimeZone zone ->
            ( { model | timeZone = zone }, Cmd.none )

        GotSession session ->
            if Session.isGuest session then
                ( { model | session = session }
                , Route.replaceUrl (Session.navKey model.session) Route.Login
                )

            else
                ( { model | session = session }
                , Route.replaceUrl (Session.navKey model.session) Route.Capture
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



-- View


view : Model -> Page.PageStructure Msg
view model =
    { title = "Timers"
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
                , el [ centerX, width (fill |> maximum 800) ] (paragraph [] [ text model.capture.text ])
                , showTimers model.timeZone model.capture.timers
                ]
        ]
    }


showTimers : Time.Zone -> List Timer -> Element Msg
showTimers zone timers =
    column [ centerX, spacing 20 ] <| List.map (showTimer zone) timers


showTimer : Time.Zone -> Timer -> Element Msg
showTimer zone timer =
    case timer.stoppedAt of
        Nothing ->
            el [ color UI.teal ] (text <| formatPosix timer.startedAt zone)

        Just stoppedAt ->
            el [] (text <| formatPosix timer.startedAt zone ++ " - " ++ formatPosix stoppedAt zone)


toSession : Model -> Session
toSession model =
    model.session


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changeSession GotSession (Session.navKey model.session)


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


formatPosix : Time.Posix -> Time.Zone -> String
formatPosix posix zone =
    let
        day =
            String.padLeft 2 '0' <| String.fromInt <| Time.toDay zone posix

        month =
            monthToString <| Time.toMonth zone posix

        year =
            String.padLeft 2 '0' <| String.fromInt <| Time.toYear zone posix

        hour =
            String.padLeft 2 '0' <| String.fromInt <| Time.toHour zone posix

        minute =
            String.padLeft 2 '0' <| String.fromInt <| Time.toMinute zone posix

        second =
            String.padLeft 2 '0' <| String.fromInt <| Time.toSecond zone posix
    in
    day
        ++ " "
        ++ month
        ++ " "
        ++ year
        ++ " "
        ++ hour
        ++ ":"
        ++ minute
        ++ ":"
        ++ second


monthToString : Time.Month -> String
monthToString month =
    case month of
        Time.Jan ->
            "Jan"

        Time.Feb ->
            "Feb"

        Time.Mar ->
            "Mar"

        Time.Apr ->
            "Apr"

        Time.May ->
            "May"

        Time.Jun ->
            "Jun"

        Time.Jul ->
            "Jul"

        Time.Aug ->
            "Aug"

        Time.Sep ->
            "Sep"

        Time.Oct ->
            "Oct"

        Time.Nov ->
            "Nov"

        Time.Dec ->
            "Dec"
