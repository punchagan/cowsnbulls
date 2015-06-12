import Html exposing (text, Attribute, div, Html, input)
import Html.Attributes as Attr exposing (style, placeholder, value, maxlength)
import Html.Events exposing (on, targetValue)
import String
import Char


-- VIEW

view : String -> Html
view string =
  let field =
        input
          [ placeholder "Guess"
          , value string
          , maxlength 4
          , on "input" targetValue (Signal.message guess.address)
          , inputStyle
          ]
          []

      footer =
        div [ footerStyle ] [ text "Made with <3 in Bangalore. In memory of the best cows & bulls player I've known." ]
  in
      div [] [field, footer]

-- STYLES

inputStyle : Attribute
inputStyle =
  style
    [ ("width", "100%")
    , ("height", "40px")
    , ("padding", "10px 0")
    , ("font-size", "2em")
    , ("text-align", "center")
    ]

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

-- WIRING

main = Signal.map view guess.signal

guess : Signal.Mailbox String
guess =
  Signal.mailbox ""

checkGuess : String -> String
checkGuess guess =
  let message =
        if String.length guess == 4 && String.all Char.isLower (String.toLower guess)
          then "4 bulls"
          else ""
  in
      message
