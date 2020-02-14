module Asset exposing (Image, logo, src)

import Html exposing (Attribute)
import Html.Attributes as Attr


type Image
    = Image String


logo : Image
logo =
    image "dwyl.png"


image : String -> Image
image filename =
    Image ("/assets/images/" ++ filename)


src : Image -> Attribute msg
src (Image url) =
    Attr.src url
