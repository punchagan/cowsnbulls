import Html exposing (text, Attribute, div)
import Html.Attributes as Attr exposing (style)



footerStyle : Attribute
footerStyle =
  style
    [ ("width", "100%")
    , ("padding", "10px 0")
    , ("font-size", "0.7em")
    , ("text-align", "center")
    , ("position", "absolute")
    , ("bottom", "0px")
    , ("border-top", "solid gray 1px")
    ]

main =
  div [ footerStyle ] [ text "Made with <3 in Bangalore. In memory of the best cows & bulls player I've known." ]
