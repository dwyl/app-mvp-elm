module Pages.Capture exposing (Model, Msg(..), init, subscriptions, toSession, update, view)

import Asset
import Endpoint
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as JD
import Page
import Route
import Session exposing (..)



-- Model


type alias Model =
    { session : Session
    , captures : List Capture
    , error : String
    }


type alias Capture =
    { text : String
    , completed : Bool
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( Model session [] "", getCaptures (token session) )



-- Update


type Msg
    = GotSession Session
    | GotCaptures (Result Http.Error (List Capture))


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
                            ( { model | error = "Error on authentication" }, Cmd.none )



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
                        div [ class "w-50 center" ] <| List.map (\capture -> showCapture capture) model.captures

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



-- captures decoder


capturesDecoder : JD.Decoder (List Capture)
capturesDecoder =
    JD.field "data" (JD.list captureDecoder)


captureDecoder : JD.Decoder Capture
captureDecoder =
    JD.map2 Capture
        (JD.field "text" JD.string)
        (JD.field "completed" JD.bool)



-- show capture


showCapture : Capture -> Html Msg
showCapture capture =
    div [ class "pa2" ]
        [ label
            [ class "dib pa2" ]
            [ input [ type_ "checkbox", checked capture.completed, class "mr2" ] []
            , text capture.text
            ]
        , button [ class "fr" ] [ text "Start" ]
        ]
