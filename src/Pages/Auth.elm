module Pages.Auth exposing (Model, Msg(..), TypeUrl(..), Url, authUrlsDecoder, getAuthUrls, init, showAuthUrl, subscriptions, toSession, update, urlDecoder, urlTypeDecoder, view)

import Asset exposing (..)
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
    , urls : List Url
    }


type alias Url =
    { url : String
    , typeUrl : TypeUrl
    }


type TypeUrl
    = Google
    | Github


init : Session -> ( Model, Cmd Msg )
init session =
    ( Model session [], getAuthUrls )



-- Update


type Msg
    = GotAuthUrls (Result Http.Error (List Url))
    | GotSession Session


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        GotAuthUrls result ->
            case result of
                Ok urls ->
                    ( { model | urls = urls }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        GotSession session ->
            ( { model | session = session }
            , Route.replaceUrl (Session.navKey model.session) Route.Home
            )



-- View


view : Model -> Page.PageStructure Msg
view model =
    { title = "Auth"
    , content =
        [ a [ Route.href Route.Home ] [ img [ Asset.src Asset.logo, class "center db pt2" ] [] ]
        , div [] <| List.map (\url -> showAuthUrl url) model.urls
        ]
    }


getAuthUrls : Cmd Msg
getAuthUrls =
    Http.get
        { url = Endpoint.toString Endpoint.authUrls
        , expect = Http.expectJson GotAuthUrls authUrlsDecoder
        }


authUrlsDecoder : JD.Decoder (List Url)
authUrlsDecoder =
    JD.field "data" (JD.list urlDecoder)


urlDecoder : JD.Decoder Url
urlDecoder =
    JD.map2 Url
        (JD.field "url" JD.string)
        (JD.field "type" urlTypeDecoder)


urlTypeDecoder : JD.Decoder TypeUrl
urlTypeDecoder =
    JD.string
        |> JD.andThen
            (\str ->
                case str of
                    "google" ->
                        JD.succeed Google

                    "github" ->
                        JD.succeed Github

                    _ ->
                        JD.fail "unkown type url"
            )


showAuthUrl : Url -> Html Msg
showAuthUrl url =
    let
        imgSrc =
            case url.typeUrl of
                Google ->
                    Asset.src Asset.signinGoogle

                Github ->
                    Asset.src Asset.signinGithub
    in
    div [ class "tc pa2" ]
        [ a [ href url.url ] [ img [ imgSrc ] [] ]
        ]


toSession : Model -> Session
toSession model =
    model.session


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changeSession GotSession (Session.navKey model.session)