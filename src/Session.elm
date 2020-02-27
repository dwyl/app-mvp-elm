module Session exposing (..)

{-| Represent the current user
-}

import Json.Decode exposing (..)


type Session
    = Guest
    | Session Person


type alias Person =
    { email : String
    , token : String
    }


decode : String -> Session
decode str =
    case decodeString (map2 Person (field "email" string) (field "token" string)) str of
        Ok p ->
            Session p

        _ ->
            Guest
