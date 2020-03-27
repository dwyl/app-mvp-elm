module UI exposing (buttonAttrs)

import Element as Elt
import Element.Border as EltBrd


buttonAttrs : List (Elt.Attribute msg)
buttonAttrs =
    [ Elt.centerX
    , EltBrd.solid
    , EltBrd.width 1
    , EltBrd.rounded 3
    , Elt.padding 5
    ]
