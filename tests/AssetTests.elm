module AssetTests exposing (suite)

import Asset
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Html.Attributes as Attr
import Main exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "test Asset module"
        [ test "src logo is defined" <|
            \_ ->
                Asset.src Asset.logo
                    |> Expect.equal (Attr.src "/assets/images/dwyl.png")
        ]
