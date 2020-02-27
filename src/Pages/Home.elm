module Pages.Home exposing (..)

import Asset
import Html exposing (..)
import Html.Attributes exposing (..)
import Page



-- Model


type alias Model =
    String


init : ( Model, Cmd Msg )
init =
    ( "", Cmd.none )



-- Update


type Msg
    = None


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )



-- View


view : Model -> Page.PageStructure Msg
view model =
    { title = "Home"
    , content =
        [ a [ href "/" ] [ img [ Asset.src Asset.logo, class "center db pt2" ] [] ]
        , h1 [ class "tc" ] [ text "Dwyl application" ]

        -- check session to know if login
        , if String.isEmpty "" then
            a [ href "/auth", class "tc db" ] [ text "login/signup" ]

          else
            span [ class "tc db" ] [ text <| "logged in with token: " ++ "token value" ]
        ]
    }
