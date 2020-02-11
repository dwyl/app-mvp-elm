module MainTests exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (..)
import Test exposing (..)
import Url
import Url.Builder as Builder
import Url.Parser as Parser


suite : Test
suite =
    test "a simple test" <|
        \_ ->
            let
                defaultUrl =
                    { protocol = Url.Https
                    , host = "dwyl.com"
                    , port_ = Just 443
                    , path = "/"
                    , query = Nothing
                    , fragment = Nothing
                    }

                url =
                    Maybe.withDefault defaultUrl (Url.fromString (Builder.absolute [] []))
            in
            Parser.parse routeParser url
                |> Expect.equal (Just Home)
