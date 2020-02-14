module Main exposing (Page(..), main, routeParser)

import Asset
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
    | Auth
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


routeParser : Parser.Parser (Page -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , Parser.map Auth (Parser.s "auth")
        ]


parseUrl : Url.Url -> Model -> ( Model, Cmd Msg )
parseUrl url model =
    case Parser.parse routeParser url of
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
                [ a [ href "/" ] [ img [ Asset.src Asset.logo, class "center db pt2" ] [] ]
                , h1 [ class "tc" ] [ text "Dwyl application" ]
                , a [ href "/auth", class "tc db" ] [ text "login/signup" ]
                ]

            Auth ->
                [ a [ href "/" ] [ img [ Asset.src Asset.logo, class "center db pt2" ] [] ]
                , h1 [ class "tc" ] [ text "login page" ]
                ]

            NotFound ->
                [ a [ href "/" ] [ img [ Asset.src Asset.logo, class "center db pt2" ] [] ]
                , h1 [ class "tc" ] [ text "page not found" ]
                ]
    }
