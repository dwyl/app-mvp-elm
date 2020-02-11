module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Url
import Url.Parser as Parser


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
    | Login
    | NotFound


type alias Model =
    { key : Nav.Key
    , page : Page
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    parseUrl url (Model key NotFound)



-- UPDATE


type Msg
    = UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest


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


parseUrl : Url.Url -> Model -> ( Model, Cmd Msg )
parseUrl url model =
    let
        parser =
            Parser.oneOf
                [ Parser.map Home Parser.top
                , Parser.map Login (Parser.s "login")
                ]
    in
    case Parser.parse parser url of
        Just page ->
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
                [ a [ href "/login" ] [ text "login/signup" ]
                , h1 [] [ text "Dwyl application" ]
                ]

            Login ->
                [ a [ href "/" ] [ text "home" ]
                , h1 [] [ text "login page" ]
                ]

            NotFound ->
                [ h1 [] [ text "page not found" ] ]
    }
