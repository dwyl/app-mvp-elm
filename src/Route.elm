module Route exposing (Route(..), fromUrl, href, replaceUrl, routeParser, routeToString)

{-| Parse a url to a Route type
see <https://guide.elm-lang.org/webapps/url_parsing.html>
-}

import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), (<?>))
import Url.Parser.Query as Query


type Route
    = Home
    | Auth (Maybe String)
    | Logout
    | Capture
    | CaptureTimers Int


routeParser : Parser.Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , Parser.map Auth (Parser.s "auth" <?> Query.string "jwt")
        , Parser.map Logout (Parser.s "logout")
        , Parser.map CaptureTimers (Parser.s "capture" </> Parser.int)
        , Parser.map Capture (Parser.s "capture")
        ]


fromUrl : Url -> Maybe Route
fromUrl url =
    Parser.parse routeParser url


href : Route -> Attribute msg
href targetRoute =
    Attr.href (routeToString targetRoute)


routeToString : Route -> String
routeToString route =
    case route of
        Home ->
            "/"

        Auth Nothing ->
            "/auth"

        Auth (Just jwt) ->
            "/auth?jwt=" ++ jwt

        Logout ->
            "/logout"

        Capture ->
            "/capture"

        CaptureTimers idCapture ->
            "/capture/" ++ String.fromInt idCapture


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToString route)
