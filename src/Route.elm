module Route exposing (fromUrl, Route(..), routeParser)

{-| Parse a url to a Route type
see <https://guide.elm-lang.org/webapps/url_parsing.html>
-}

import Url exposing (Url)
import Url.Parser as Parser exposing ((<?>))
import Url.Parser.Query as Query


type Route
    = Home
    | Auth (Maybe String)


routeParser : Parser.Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , Parser.map Auth (Parser.s "auth" <?> Query.string "jwt")
        ]


fromUrl : Url -> Maybe Route
fromUrl url =
    Parser.parse routeParser url
