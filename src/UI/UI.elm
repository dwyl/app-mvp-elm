module UI.UI exposing (buttonAttrs, completedButtonAttrs, darkGrey, dwylLogo, lightGrey, loader, loaderHtml, mint, mintButtonAttrs, startButtonAttrs, stopButtonAttrs, teal)

import Asset
import Element as Elt
import Element.Background as EltBackground
import Element.Border as EltBrd
import Element.Font as EltFont
import Html
import Html.Attributes as HtmlAttr
import Route



-- logo


dwylLogo : Elt.Element msg
dwylLogo =
    Elt.el [ Elt.centerX, Elt.width (Elt.px 150), Elt.padding 10 ]
        (Elt.link []
            { url = Route.routeToString Route.Capture
            , label = Elt.image [ Elt.centerX ] { src = Asset.imagePath Asset.logo, description = "DWYL Logo" }
            }
        )



-- loading spinner


loader : Elt.Element msg
loader =
    Elt.el [ Elt.htmlAttribute <| HtmlAttr.class "loader" ] Elt.none


loaderHtml : Html.Html msg
loaderHtml =
    Html.div [ HtmlAttr.class "loader" ] []



-- Buttons


buttonAttrs : List (Elt.Attribute msg)
buttonAttrs =
    [ Elt.centerX
    , EltBrd.solid
    , EltBrd.width 1
    , EltBrd.rounded 3
    , Elt.padding 5
    ]


mintButtonAttrs : List (Elt.Attribute msg)
mintButtonAttrs =
    [ Elt.centerX
    , EltBrd.rounded 3
    , Elt.padding 15
    , EltBackground.color mint
    , EltFont.color white
    ]


startButtonAttrs : List (Elt.Attribute msg)
startButtonAttrs =
    [ Elt.centerX
    , EltBrd.rounded 3
    , Elt.padding 15
    , EltBackground.color teal
    , EltFont.color white
    ]


stopButtonAttrs : List (Elt.Attribute msg)
stopButtonAttrs =
    [ Elt.centerX
    , EltBrd.rounded 3
    , Elt.padding 15
    , EltBackground.color orange
    , EltFont.color white
    ]


completedButtonAttrs : List (Elt.Attribute msg)
completedButtonAttrs =
    [ Elt.centerX
    , EltBrd.rounded 3
    , Elt.padding 15
    , EltBackground.color grey
    , EltFont.color white
    ]



-- Colours


mint : Elt.Color
mint =
    Elt.rgb255 75 192 169


white : Elt.Color
white =
    Elt.rgb255 255 255 255


teal : Elt.Color
teal =
    Elt.rgb255 26 117 102


orange : Elt.Color
orange =
    Elt.rgb255 252 134 91


lightGrey : Elt.Color
lightGrey =
    Elt.rgb255 202 210 211


grey : Elt.Color
grey =
    Elt.rgb255 113 126 132


darkGrey : Elt.Color
darkGrey =
    Elt.rgb255 69 83 91
