import Html exposing (text, Attribute, div, Html, input)
import Html.Attributes as Attr exposing (style, placeholder, value, maxlength, id, autofocus, name)
import Html.Events exposing (on, targetValue, keyCode)
import String
import Char
import List
import Set
import Json.Decode as Json
import Signal exposing (Signal, Address)
import Html.Lazy exposing (lazy, lazy2, lazy3)

-- MODEL

type alias Model =
    { word: String
    , guess: String
    , result: (Int, Int)
    }

initialModel = { word = "word", guess = "", result = (0, 0)}

-- UPDATE

type Action =
  NoOp
  | Guess
  | UpdateGuess String

update : Action -> Model -> Model
update action model =
    case action of
      NoOp -> model

      UpdateGuess str ->
        { model |
            guess <- str,
            result <- (0, 0)
         }

      Guess ->
          { model |
              result <- checkGuess model.word model.guess
          }


-- VIEW

view : Address Action -> Model -> Html
view address model =
  let result =
        div [inputStyle] [ text <| toString model.result ]

  in
      div
        []
        [ lazy2 guessWord address model.guess
        , result
        , footer
        ]


onEnter : Address a -> a -> Attribute
onEnter address value =
    on "keydown"
      (Json.customDecoder keyCode is13)
      (\_ -> Signal.message address value)


is13 : Int -> Result String ()
is13 code =
  if code == 13 then Ok () else Err "not the right key code"

guessWord : Address Action -> String -> Html
guessWord address guess =
      input
        [ id "guess"
        , placeholder "Guess"
        , maxlength 4
        , autofocus True
        , value guess
        , name "guess"
        , on "input" targetValue (Signal.message address << UpdateGuess)
        , onEnter address Guess
        , inputStyle
        ]
        []



footer : Html
footer = div [ footerStyle ] [ text "Made with <3 in Bangalore. In memory of the best cows & bulls player I've known." ]


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

-- SIGNALS

main : Signal Html
main =
  Signal.map (view actions.address) model


-- manage the model of our application over time
model : Signal Model
model =
  Signal.foldp update initialModel actions.signal

-- actions from user input
actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp


guess : Signal.Mailbox String
guess =
  Signal.mailbox ""

checkGuess : String -> String -> (Int, Int)
checkGuess word guess =
  if (  String.length guess == String.length word
     && String.length word == (String.toList guess |> Set.fromList |> Set.toList |> List.length)
     && String.all Char.isLower (String.toLower guess)
     )
    then
      let guess_set = String.toList guess |> Set.fromList
          word_set = String.toList word |> Set.fromList
          total = Set.intersect word_set guess_set |> Set.toList |> List.length
          bulls = String.filter (\x -> let y = String.fromChar x in String.indexes y word == String.indexes y guess) guess |> String.length
      in (bulls, total - bulls)
    else (0, 0)
