module Page exposing (..)

{-| Define the structure of a page
This module can contain the future header and footer content
-}

import Browser
import Html exposing (Html, map)


type alias PageStructure msg =
    { title : String
    , content : List (Html msg)
    }


view : (a -> msg) -> PageStructure a -> Browser.Document msg
view toMsg structure =
    { title = structure.title
    , body = List.map (Html.map toMsg) structure.content
    }
