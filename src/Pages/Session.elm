module Pages.Session exposing (Model, Msg(..), PersonInfo, getPersonInfo, init, personDecoder, subscriptions, toSession, update, view)

import Asset
import Endpoint
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as JD
import Page
import Route
import Session exposing (Session)


type alias Model =
    { session : Session
    , token : String
    }


type alias PersonInfo =
    { email : String
    , name : String
    }


init : Session -> String -> ( Model, Cmd Msg )
init session token =
    ( Model session token, getPersonInfo token )



-- Update


type Msg
    = GotPersonInfo (Result Http.Error PersonInfo)
    | GotSession Session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPersonInfo result ->
            case result of
                Ok person ->
                    let
                        session =
                            { email = person.email, token = model.token }
                    in
                    ( model, Session.storeSession (Just <| Session.encode session) )

                -- if a 401 redirect to 401 page not authorised
                Err e ->
                    ( model, Cmd.none )

        GotSession session ->
            ( { model | session = session }
            , Route.replaceUrl (Session.navKey model.session) Route.Home
            )


getPersonInfo : String -> Cmd Msg
getPersonInfo token =
    -- make sure to update the backend app endpoint to return a user info
    Http.request
        { method = "GET"
        , headers = [ Http.header "authorization" ("Bearer " ++ token) ]
        , url = Endpoint.toString Endpoint.personInfo
        , body = Http.emptyBody
        , expect = Http.expectJson GotPersonInfo personDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


personDecoder : JD.Decoder PersonInfo
personDecoder =
    JD.field "data"
        (JD.map2 PersonInfo
            (JD.field "email" JD.string)
            (JD.field "name" JD.string)
        )



-- View


view : Model -> Page.PageStructure Msg
view _ =
    { title = "Auth"
    , content =
        [ img [ Asset.src Asset.logo, class "center db pt2" ] []
        , p [ class "tc" ] [ text "Creating session..." ]
        ]
    }



-- listen to change of cache


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changeSession GotSession (Session.navKey model.session)


toSession : Model -> Session
toSession model =
    model.session
