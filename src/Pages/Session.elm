module Pages.Session exposing (Model, Msg(..), Person, getPersonInfo, init, personDecoder, subscriptions, toSession, update, view)

import Asset
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as JD
import Page
import Session exposing (..)


type alias Model =
    { session : Session
    , token : String
    }


type alias Person =
    { email : String
    , name : String
    }


init : Session -> String -> ( Model, Cmd Msg )
init session token =
    -- insstead of Cmd.none, get user info
    ( Model session token, Cmd.none )



-- Update


type Msg
    = GotPersonInfo (Result Http.Error Person)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPersonInfo result ->
            case result of
                Ok person ->
                    -- instead of Cmd.none, call a store cache command
                    ( model, Cmd.none )

                -- if a 401 redirect to 401 page not authorised
                Err _ ->
                    ( model, Cmd.none )


getPersonInfo : Cmd Msg
getPersonInfo =
    -- make sure to update the backend app endpoint to return a user info
    -- need to pass the jwt as a header in the request
    Http.get
        { url = "https://appapispike.herokuapp.com/api/person/info"
        , expect = Http.expectJson GotPersonInfo personDecoder
        }


personDecoder : JD.Decoder Person
personDecoder =
    JD.map2 Person
        (JD.field "email" JD.string)
        (JD.field "name" JD.string)



-- View


view : Model -> Page.PageStructure Msg
view model =
    { title = "Auth"
    , content =
        [ img [ Asset.src Asset.logo, class "center db pt2" ] []
        , p [ class "tc" ] [ text "Creating session..." ]
        ]
    }



-- listen to change of cache


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


toSession : Model -> Session
toSession model =
    model.session
