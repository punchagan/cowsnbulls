import Char
import Html exposing (text, Attribute, div, Html, input, button)
import Html.Attributes as Attr exposing (style, placeholder, value, maxlength, id, autofocus, name)
import Html.Events exposing (on, onClick, targetValue, keyCode)
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Json.Decode as Json
import List
import Set
import Signal exposing (Signal, Address)
import String

-- MODEL

type alias Model =
    { word: String
    , input: String
    , guess: String
    , result: (Int, Int)
    }

initialModel = { word = "word", input = "", guess="", result = (0, 0) }

-- UPDATE

type Action =
  NoOp
  | Guess
  | UpdateInput String
  | Restart

update : Action -> Model -> Model
update action model =
    case action of
      NoOp -> model

      UpdateInput str ->
        { model |
            input <- str,
            guess <- ""
         }

      Guess ->
        { model |
            guess <- if validGuess model.word model.input then model.input else "",
            result <- checkGuess model.word model.input,
            input <- ""
        }

      Restart -> initialModel


-- VIEW

view : Address Action -> Model -> Html
view address model =
  let result =
        div [inputStyle] [ (if model.guess == ""
                            then ""
                            else model.guess ++ " -- " ++ toString model.result)
                           |> text
                         ]

  in
      div
        []
        [ lazy2 guessWord address model.input
        , result
        , button [ onClick address Restart, restartStyle ] [ text "Restart" ]
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
        , placeholder "Guess a 4-letter word."
        , maxlength 4
        , autofocus True
        , value guess
        , name "guess"
        , on "input" targetValue (Signal.message address << UpdateInput)
        , onEnter address Guess
        , inputStyle
        ]
        []



footer : Html
footer = div [ footerStyle ] [ text "Built with <3 in Bangalore. In memory of the best cows & bulls player I've known." ]


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

restartStyle : Attribute
restartStyle =
  style
    [ ("height", "40px")
    , ("padding", "10px")
    , ("margin-left", "48%")
    , ("font-size", "1em")
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

checkGuess : String -> String -> (Int, Int)
checkGuess word guess =
  if validGuess word guess
    then
      let guess_set = String.toList guess |> Set.fromList
          word_set = String.toList word |> Set.fromList
          total = Set.intersect word_set guess_set |> Set.toList |> List.length
          bulls = String.filter (\x -> let y = String.fromChar x in String.indexes y word == String.indexes y guess) guess |> String.length
      in (bulls, total - bulls)
    else (0, 0)

validGuess : String -> String -> Bool
validGuess word guess =
  (  String.length guess == String.length word
  && String.length word == (String.toList guess |> Set.fromList |> Set.toList |> List.length)
  && String.all Char.isLower (String.toLower guess)
  )
