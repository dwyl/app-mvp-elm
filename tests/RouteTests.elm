module RouteTests exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Route
import Test exposing (..)
import Url
import Url.Parser as Parser


suite : Test
suite =
    describe "Testing routes"
        [ test "Test home page" <|
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
                        Maybe.withDefault defaultUrl (Url.fromString "http://localhost/")
                in
                Parser.parse Route.routeParser url
                    |> Expect.equal (Just Route.Home)
        , test "Test auth page" <|
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
                        Maybe.withDefault defaultUrl (Url.fromString "http://locahost/auth")
                in
                Parser.parse Route.routeParser url
                    |> Expect.equal (Just (Route.Auth Nothing))
        , test "Test auth page with jwt" <|
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
                        Maybe.withDefault defaultUrl (Url.fromString "http://locahost/auth?jwt=aaa.bbb.ccc")
                in
                Parser.parse Route.routeParser url
                    |> Expect.equal (Just (Route.Auth (Just "aaa.bbb.ccc")))
        , test "Test 404 page" <|
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
                        Maybe.withDefault defaultUrl (Url.fromString "http://locahost/wrong-page")
                in
                Parser.parse Route.routeParser url
                    |> Expect.equal Nothing
        , test "Test logout route" <|
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
                        Maybe.withDefault defaultUrl (Url.fromString "http://locahost/logout")
                in
                Parser.parse Route.routeParser url
                    |> Expect.equal (Just Route.Logout)
        , test "Test capture route" <|
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
                        Maybe.withDefault defaultUrl (Url.fromString "http://locahost/capture")
                in
                Parser.parse Route.routeParser url
                    |> Expect.equal (Just Route.Capture)
        ]
