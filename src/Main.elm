module Main exposing (Page(..), main, routeParser)

import Asset
import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as JD
import Url
import Url.Parser as Parser exposing ((<?>))
import Url.Parser.Query as Query


main : Program () Model Msg
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


type Page
    = Home
    | Auth (Maybe String)
    | NotFound


type alias Model =
    { key : Nav.Key
    , page : Page
    , authUrls : List Url
    , token : String
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    parseUrl url (Model key NotFound [] "")



-- UPDATE


type Msg
    = UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | GotAuthUrls (Result Http.Error (List Url))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked link ->
            case link of
                Browser.Internal urlRequested ->
                    ( model, Nav.pushUrl model.key (Url.toString urlRequested) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            parseUrl url model

        GotAuthUrls result ->
            case result of
                Ok urls ->
                    ( { model | authUrls = urls }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )


routeParser : Parser.Parser (Page -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , Parser.map Auth (Parser.s "auth" <?> Query.string "jwt")
        ]


parseUrl : Url.Url -> Model -> ( Model, Cmd Msg )
parseUrl url model =
    case Parser.parse routeParser url of
        Just page ->
            case page of
                Auth Nothing ->
                    ( { model | page = page }, getAuthUrls )

                Auth (Just jwt) ->
                    ( { model | token = jwt }, Nav.pushUrl model.key "/" )

                -- redirect to home page
                _ ->
                    ( { model | page = page }, Cmd.none )

        Nothing ->
            ( { model | page = NotFound }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Browser.Document Msg
view model =
    { title = "DWYL App"
    , body =
        case model.page of
            Home ->
                [ a [ href "/" ] [ img [ Asset.src Asset.logo, class "center db pt2" ] [] ]
                , h1 [ class "tc" ] [ text "Dwyl application" ]
                , if String.isEmpty model.token then
                    a [ href "/auth", class "tc db" ] [ text "login/signup" ]

                  else
                    span [ class "tc db" ] [ text <| "logged in with token: " ++ model.token ]
                ]

            Auth _ ->
                [ a [ href "/" ] [ img [ Asset.src Asset.logo, class "center db pt2" ] [] ]
                , div [] <| List.map (\url -> showAuthUrl url) model.authUrls
                ]

            NotFound ->
                [ a [ href "/" ] [ img [ Asset.src Asset.logo, class "center db pt2" ] [] ]
                , h1 [ class "tc" ] [ text "page not found" ]
                ]
    }



-- OAuth urls
-- convert oauth login urls (github, google) to Elm Url type


type alias Url =
    { url : String
    , typeUrl : TypeUrl
    }


type TypeUrl
    = Google
    | Github


getAuthUrls : Cmd Msg
getAuthUrls =
    Http.get
        { url = "https://appapispike.herokuapp.com/api/auth/urls"
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
