import Char
import Html exposing (a, Attribute, button, div, Html, input, text)
import Html.Attributes as Attr
        exposing (autofocus, href, id, maxlength, name, placeholder, style, target, value)
import Html.Events exposing (keyCode, on, onClick, targetValue)
import Html.Lazy exposing (lazy2)
import Http
import Json.Decode as Json
import List
import Maybe
import Random
import Set
import Signal exposing ((<~), Signal, Address)
import String
import Task exposing (andThen, Task)
import Time exposing (every, millisecond)

-- MODEL

type alias Model =
    { word: String
    , input: String
    , guess: String
    , result: (Int, Int)
    , words: List String
    , guess_count: Int
    , guessed: Bool
    }

emptyModel = { word = ""
             , input = ""
             , guess=""
             , guess_count = 0
             , result = (0, 0)
             , words = []
             , guessed = False
             }

-- UPDATE

type Action =
  NoOp
  | Guess
  | UpdateInput String
  | UpdateWords (List String)
  | PickWord Random.Seed
  | ShowWord

update : Action -> Model -> Model
update action model =
    case action of
      NoOp -> model

      UpdateInput str ->
        { model |
                  input <- str,
                  guess <- "",
                  guessed <- False
         }

      Guess ->
          let valid = validWord model.words model.input
          in
            if valid
            then { model |
                           guess <- model.input,
                           result <- checkGuess model.word model.input,
                           input <- "",
                           guess_count <- model.guess_count + 1,
                           guessed <- model.input == model.word

                 }
            else model

      UpdateWords words ->
          { emptyModel |
                         words <- words
          }

      ShowWord ->
          {
            emptyModel |
                         guessed <- True,
                         words <- model.words,
                         word <- model.word
          }

      PickWord seed ->
          { emptyModel |
                         word <- getRandomItem seed model.words |> Maybe.withDefault "",
                         words <- model.words
          }

-- VIEW

view : Address Action -> Model -> Html
view address model =
    let
        restart_button =
            button
            [ onClick pickWord.address 1 ]
            [ text "Restart" ]

        dict_div =
            div
            [ inputStyle ]
            [ ( a
                [ href ("http://www.dict.org/bin/Dict?Form=Dict2&Database=*&Query=" ++ model.word)
                , target "_blank"
                ]
                [ text <| "See meaning: " ++ model.word ]
              ),
              div [] [restart_button]
            ]

        show_result =
            \(bulls, cows) -> toString bulls ++ " bulls, " ++ toString cows ++ " cows"

        result =
            div [inputStyle] [ text <|
                                    if model.guess == ""
                                    then ""
                                    else (show_result model.result) ++ " in \"" ++ model.guess ++ "\""
                             ]
        guessed = if (model.guessed && model.guess_count > 0) then "Hooray! Guessed in " else ""
        guess_count =
            div [inputStyle] [ text <|
                                    guessed ++ if model.guess_count > 0
                                    then toString model.guess_count ++
                                         if model.guess_count == 1 then " guess" else " guesses"
                                    else ""
                             ]
    in
      div
      []
      [ if model.guessed then dict_div else lazy2 guessWord address model
      , result
      , guess_count
      , button
        [ onClick address ShowWord
        , if model.guess_count > 0 then restartStyle else hideStyle
        ]
        [ text "Give up" ]
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

hideStyle : Attribute
hideStyle =
  style
  [ ("display", "None") ]

footerStyle : Attribute
footerStyle =
  style
    [ ("width", "100%")
    , ("padding", "10px 0")
    , ("font-size", "1em")
    , ("opacity", "0.5")
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

clockSeed : Signal Random.Seed
clockSeed = (\time -> Random.initialSeed (round time)) <~ (every millisecond)

pickWord : Signal.Mailbox Int
pickWord =
    Signal.mailbox 1

port getWords : Task Http.Error ()
port getWords =
    let
        -- fixme: doesn't work when running from /path/index.html
        url = "/word_list_4.txt"
    in
      Http.getString url
              `andThen` \content -> Signal.send word_db.address (content |> String.trim |> String.lines)
              `andThen` \_ -> (Signal.send pickWord.address 1)

-- actions from user input
actions : Signal.Mailbox Action
actions =
    Signal.mailbox NoOp

updateEvents : Signal Action
updateEvents =
    Signal.mergeMany [
               actions.signal,
               Signal.map UpdateWords word_db.signal,
               Signal.map PickWord (Signal.sampleOn pickWord.signal clockSeed)
              ]

-- HELPERS

checkGuess : String -> String -> (Int, Int)
checkGuess word guess =
    let guess_set = String.toList guess |> Set.fromList
        word_set = String.toList word |> Set.fromList
        total = Set.intersect word_set guess_set |> Set.toList |> List.length
        bulls = String.filter (\x -> let y = String.fromChar x in String.indexes y word == String.indexes y guess) guess |> String.length
    in (bulls, total - bulls)

validWord : (List String) -> String -> Bool
validWord words guess =
    List.member guess words

getRandomItem : Random.Seed -> List a -> Maybe a
getRandomItem seed xs =
    let n = List.length xs
        (m, _) = Random.generate (Random.int 0 n) seed
    in
      List.head <| List.drop m xs
