import Char
import Html exposing (Attribute, button, div, Html, input, text)
import Html.Attributes as Attr exposing (autofocus, id, maxlength, name, placeholder, style, value)
import Html.Events exposing (keyCode, on, onClick, targetValue)
import Html.Lazy exposing (lazy2)
import Http
import Json.Decode as Json
import List
import Maybe
import Random
import Set
import Signal exposing (Signal, Address)
import String
import Task exposing (Task, andThen)

-- MODEL

type alias Model =
    { word: String
    , input: String
    , guess: String
    , result: (Int, Int)
    , words: List String
    }

emptyModel = { word = ""
             , input = ""
             , guess=""
             , result = (0, 0)
             , words = []
             }

-- UPDATE

type Action =
  NoOp
  | Guess
  | UpdateInput String
  | UpdateWords (List String)
  | PickWord String

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

      UpdateWords words ->
          { model |
                    words <- words
          }

      PickWord word ->
          { emptyModel |
                         word <- word,
                         words <- model.words
          }

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
        [ lazy2 guessWord address model
        , result
        , button
          [ onClick address (model.words |> getRandomItem 550 |> Maybe.withDefault "" |> PickWord)
          , restartStyle
          ]
          [ text "Restart" ]
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

guessWord : Address Action -> Model -> Html
guessWord address model =
      input
        [ id "guess"
        , placeholder ( if (List.length model.words > 0 && (String.length model.word == 4))
                        then "Guess a 4-letter word."
                        else "Wait, Downloading word-list..."
                      )
        , maxlength 4
        , autofocus True
        , value model.input
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
  Signal.foldp update emptyModel updateEvents

word_db : Signal.Mailbox (List String)
word_db =
    Signal.mailbox []

pickedWord : Signal.Mailbox String
pickedWord =
    Signal.mailbox ""

port getWords : Task Http.Error ()
port getWords =
    let
        url = "/word_list_4.txt"
    in
      Http.getString url
              `andThen` \content -> Signal.send word_db.address (String.lines content)
              `andThen` \_ -> (String.lines content |> getRandomItem 0 |> Maybe.withDefault "" |> Signal.send pickedWord.address)

-- actions from user input
actions : Signal.Mailbox Action
actions =
    Signal.mailbox NoOp

updateEvents : Signal Action
updateEvents =
    Signal.mergeMany [
               actions.signal,
               Signal.map UpdateWords word_db.signal,
               Signal.map PickWord pickedWord.signal
              ]

-- HELPERS

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

getRandomItem : Int -> List a -> Maybe a
getRandomItem seed xs =
    let n = List.length xs
        (m, _) = Random.generate (Random.int 0 n) (Random.initialSeed seed)
    in
      List.head <| List.drop m xs
