port module Session exposing (Person, Session(..), changeSession, decode, encode, isGuest, logout, navKey, onSessionChange, storeSession, token)

{-| Represent the current user
The user can be authenticated or a guest
-}

import Browser.Navigation as Nav
import Json.Decode as JD
import Json.Encode as JE


type Session
    = Guest Nav.Key
    | Session Nav.Key Person


type alias Person =
    { email : String
    , token : String
    }


decode : Nav.Key -> String -> Session
decode key str =
    case JD.decodeString (JD.map2 Person (JD.field "email" JD.string) (JD.field "token" JD.string)) str of
        Ok p ->
            Session key p

        _ ->
            Guest key


encode : Person -> JE.Value
encode person =
    JE.object
        [ ( "email", JE.string person.email )
        , ( "token", JE.string person.token )
        ]


navKey : Session -> Nav.Key
navKey session =
    case session of
        Guest key ->
            key

        Session key _ ->
            key


token : Session -> String
token session =
    case session of
        Guest _ ->
            ""

        Session _ person ->
            person.token


port storeSession : Maybe JD.Value -> Cmd msg


port onSessionChange : (JE.Value -> msg) -> Sub msg


changeSession : (Session -> msg) -> Nav.Key -> Sub msg
changeSession toMsg key =
    changePerson (\maybePerson -> toMsg (sessionFromPerson maybePerson key))


changePerson : (Maybe Person -> msg) -> Sub msg
changePerson toMsg =
    onSessionChange (\value -> toMsg (decodeFromChange personDecoder value))


sessionFromPerson : Maybe Person -> Nav.Key -> Session
sessionFromPerson maybePerson key =
    case maybePerson of
        Just person ->
            Session key person

        Nothing ->
            Guest key


personDecoder : JD.Decoder Person
personDecoder =
    JD.map2 Person
        (JD.field "email" JD.string)
        (JD.field "token" JD.string)


decodeFromChange : JD.Decoder Person -> JD.Value -> Maybe Person
decodeFromChange decoder val =
    JD.decodeValue decoder val
        |> Result.toMaybe



-- set value in localStorage to null


logout : Cmd msg
logout =
    storeSession Nothing


isGuest : Session -> Bool
isGuest session =
    case session of
        Guest _ ->
            True

        _ ->
            False
