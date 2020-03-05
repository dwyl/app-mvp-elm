module Endpoint exposing (Endpoint, authUrls, personInfo, toString, url)

import Url.Builder exposing (QueryParameter)


{-| This module manages the creation of the api urls
see <https://package.elm-lang.org/packages/elm/url/latest/Url-Builder>
to learn how to create string url with the module Url.Builder
-}
type Endpoint
    = Endpoint String


toString : Endpoint -> String
toString (Endpoint urlEndpoint) =
    urlEndpoint


url : List String -> List QueryParameter -> Endpoint
url path queryParams =
    -- "https://dwyl-app-api.herokuapp.com"
    -- "http://localhost:4000"
    Url.Builder.crossOrigin "https://dwyl-app-api.herokuapp.com" path queryParams
        |> Endpoint



-- auth urls


authUrls : Endpoint
authUrls =
    url [ "api", "login" ] []


personInfo : Endpoint
personInfo =
    url [ "api", "person", "info" ] []
