module Asset exposing (Image, imagePath, logo, signinGithub, signinGoogle, src)

import Html exposing (Attribute)
import Html.Attributes as Attr


type Image
    = Image String


logo : Image
logo =
    image "dwyl.png"


signinGoogle : Image
signinGoogle =
    image "signin-google.png"


signinGithub : Image
signinGithub =
    image "signin-github.png"


image : String -> Image
image filename =
    Image ("/assets/images/" ++ filename)


imagePath : Image -> String
imagePath (Image url) =
    url


src : Image -> Attribute msg
src (Image url) =
    Attr.src url
