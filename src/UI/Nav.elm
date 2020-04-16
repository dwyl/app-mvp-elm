module UI.Nav exposing (Config(..), State(..), config, init, isOpen, toggleNav, view)

import Element exposing (..)
import Element.Background as EltBackground
import Element.Events exposing (onClick)
import Element.Font exposing (..)
import Route
import Session exposing (Session, isGuest)
import UI.UI as UI


type State
    = State Bool


type Config msg
    = Config
        { toMsg : State -> msg
        , session : Session
        }


config : { toMsg : State -> msg, session : Session } -> Config msg
config { toMsg, session } =
    Config { toMsg = toMsg, session = session }


init : State
init =
    State False


toggleNav : State -> State
toggleNav (State navState) =
    State (not navState)


isOpen : State -> Bool
isOpen (State b) =
    b


view : Config msg -> State -> Element msg
view (Config { toMsg, session }) state =
    row [ height fill, width fill ]
        [ el
            [ height fill
            , width fill
            , transparent True
            , onClick (toMsg (toggleNav state))
            , EltBackground.color UI.mint
            ]
            none
        , el
            [ height fill
            , width (fill |> minimum 500)
            , Element.alignRight
            , center
            , EltBackground.color UI.lightGrey
            ]
            (column [ width fill, padding 50, spacing 20 ]
                [ if isGuest session then
                    link
                        [ centerX, color UI.teal, bold ]
                        { url = Route.routeToString (Route.Auth Nothing)
                        , label = text "login/signup"
                        }

                  else
                    link
                        [ centerX, color UI.teal, bold, underline ]
                        { url = Route.routeToString Route.Logout
                        , label = text "logout"
                        }
                ]
            )
        ]
