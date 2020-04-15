module Pages.Session exposing (Model, Msg(..), PersonInfo, getPersonInfo, init, personDecoder, subscriptions, toSession, update, view)

import Asset
import Element exposing (..)
import Endpoint
import Http
import Json.Decode as JD
import Page
import Route
import Session exposing (Session)


type alias Model =
    { session : Session
    , token : String
    , error : String
    }


type alias PersonInfo =
    { email : String
    , name : String
    , avatar : String
    }


init : Session -> String -> ( Model, Cmd Msg )
init session token =
    ( Model session token "", getPersonInfo token )



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
                            { email = person.email, token = model.token, avatar = person.avatar }
                    in
                    ( model, Session.storeSession (Just <| Session.encode session) )

                Err httpError ->
                    case httpError of
                        Http.BadStatus 401 ->
                            ( { model | error = "Access not authorised" }
                            , Cmd.batch
                                [ Session.logout
                                , Route.replaceUrl (Session.navKey model.session) Route.Login
                                ]
                            )

                        Http.BadStatus 404 ->
                            ( { model | error = "User information can't be retrieved" }, Cmd.none )

                        _ ->
                            ( { model | error = "Error on authentication" }, Cmd.none )

        GotSession session ->
            if Session.isGuest session then
                ( { model | session = session }
                , Route.replaceUrl (Session.navKey model.session) Route.Login
                )

            else
                ( { model | session = session }
                , Route.replaceUrl (Session.navKey model.session) Route.Capture
                )


getPersonInfo : String -> Cmd Msg
getPersonInfo token =
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
        (JD.map3 PersonInfo
            (JD.field "email" JD.string)
            (JD.field "name" JD.string)
            (JD.field "avatar" JD.string)
        )



-- View


view : Model -> Page.PageStructure Msg
view model =
    { title = "Auth"
    , content =
        [ layout [] <|
            column [ Element.centerX ]
                [ link [ Element.centerX ]
                    { url = Route.routeToString Route.Capture
                    , label = image [ centerX ] { src = Asset.imagePath Asset.logo, description = "DWYL Logo" }
                    }
                , if String.isEmpty model.error then
                    el [ centerX ] (text "Creating session...")

                  else
                    el [ centerX ] (text model.error)
                ]
        ]
    }



-- listen to change of cache


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changeSession GotSession (Session.navKey model.session)


toSession : Model -> Session
toSession model =
    model.session
