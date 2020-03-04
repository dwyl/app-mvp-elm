module Pages.Session exposing (Model, Msg(..), Person, getPersonInfo, init, personDecoder, subscriptions, toSession, update, view)

import Asset
import Endpoint
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as JD
import Page
import Route
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
    ( Model session token, getPersonInfo token )



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
                    let
                        _ =
                            Debug.log "Person" person
                    in
                    ( model, Route.replaceUrl (navKey model.session) Route.Home )

                -- if a 401 redirect to 401 page not authorised
                Err e ->
                    let
                        _ =
                            Debug.log "Error" e
                    in
                    ( model, Cmd.none )


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


personDecoder : JD.Decoder Person
personDecoder =
    JD.field "data"
        (JD.map2 Person
            (JD.field "email" JD.string)
            (JD.field "name" JD.string)
        )



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



-- create port module
-- create storeSession
-- in html.js store the session
-- create subscriptions to listen to new change in store and trigger the GotSession session message
-- redirect to Home page with the new session
-- Create a redirect function in Route module
