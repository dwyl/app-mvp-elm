module Session exposing (Person, Session(..), decode, navKey)

{-| Represent the current user
-}

import Browser.Navigation as Nav
import Json.Decode exposing (..)


type Session
    = Guest Nav.Key
    | Session Nav.Key Person


type alias Person =
    { email : String
    , token : String
    }


decode : Nav.Key -> String -> Session
decode key str =
    case decodeString (map2 Person (field "email" string) (field "token" string)) str of
        Ok p ->
            Session key p

        _ ->
            Guest key


navKey : Session -> Nav.Key
navKey session =
    case session of
        Guest key ->
            key

        Session key _ ->
            key
