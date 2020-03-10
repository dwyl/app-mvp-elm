module Pages.Capture exposing (Model, Msg(..), init, subscriptions, toSession, update, view)

import Asset
import Endpoint
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as JD
import Json.Encode as JE
import Page
import Route
import Session exposing (..)



-- Model


type alias Model =
    { session : Session
    , captures : List Capture
    , newCapture : Capture
    , error : String
    }


type alias Capture =
    { idCapture : Int
    , text : String
    , completed : Bool
    , disabled : Bool
    }


initCapture : Capture
initCapture =
    Capture 0 "" False False


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
                    List.map
                        (\c ->
                            if c.idCapture == idCapture then
                                { c | disabled = True }

                            else
                                c
                        )
                        model.captures
            in
            ( { model | captures = captures }, Cmd.none )



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



-- captures decoder


capturesDecoder : JD.Decoder (List Capture)
capturesDecoder =
    JD.field "data" (JD.list captureDecoder)


savedCaptureDecoder : JD.Decoder Capture
savedCaptureDecoder =
    JD.field "data" captureDecoder


captureDecoder : JD.Decoder Capture
captureDecoder =
    JD.map4 Capture
        (JD.field "capture_id" JD.int)
        (JD.field "text" JD.string)
        (JD.field "completed" JD.bool)
        (JD.succeed False)


captureEncode : Capture -> JD.Value
captureEncode capture =
    JE.object
        [ ( "text", JE.string capture.text ) ]



-- show capture


showCapture : Capture -> Html Msg
showCapture capture =
    div [ class "pa2" ]
        [ label
            [ class "dib pa2" ]
            [ input [ type_ "checkbox", checked capture.completed, disabled capture.disabled, class "mr2" ] []
            , text <| capture.text
            ]
        , button [ disabled capture.disabled, class "fr", classList [ ( "pointer", not capture.disabled ) ], onClick (StartTimer capture.idCapture) ] [ text "Start" ]
        ]
