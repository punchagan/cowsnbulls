import Html exposing (a, Attribute, button, div, footer, header, Html, input, img, text, table, tr, td, th)
import Html.Attributes as Attr
        exposing (autofocus, href, id, maxlength, name, placeholder, src, style, target, value)
import Html.Events exposing (keyCode, on, onClick, targetValue)
import Html.Lazy exposing (lazy2)
import Http
import Json.Decode as Json
import Json.Encode as Encode
import List
import Maybe
import Random
import Set
import Signal exposing ((<~), Signal, Address)
import Storage exposing (getItem, setItem)
import String
import Task exposing (andThen, Task, toMaybe)
import Time exposing (every, millisecond)

-- MODEL

type Message =
    DownloadMessage
        | StartMessage
        | GuessMessage
        | ErrorMessage
        | NoWordListMessage
        | GuessedMessage

showMessage : Message -> String
showMessage message =
    case message of
      DownloadMessage -> "Wait, Downloading word list..."
      NoWordListMessage -> "Failed to download word list."
      StartMessage -> "I thought of a 4-letter word. Guess"
      GuessMessage -> "Next guess?"
      GuessedMessage -> "You have already guessed this!"
      ErrorMessage -> "Only 'valid' 4-letter, without repetition! Should we add word to our list?"

type alias Score = (Int , Int)

type alias Model =
    { word: String
    , words: List String
    , input: String
    , guess: String
    , result: Score
    , count: Int
    , done: Bool
    , cancelled: Bool
    , message: Message
    , guesses: List (String, Score)
    , show_history: Bool
    }

emptyModel = { word = ""
             , words = []
             , input = ""
             , guess=""
             , result = (0, 0)
             , count = 0
             , done = False
             , cancelled = False
             , message = DownloadMessage
             , guesses = []
             , show_history = False
             }

-- UPDATE

type Action = NoOp
            | Guess
            | UpdateInput String
            | UpdateWords (List String)
            | PickWord Random.Seed
            | ShowWord
            | NoWordList Bool
            | ToggleShowHistory

update : Action -> Model -> Model
update action model =
    case action of
      NoOp -> model

      UpdateInput str ->
        { model |
                  input <- String.toLower str,
                  guess <- "",
                  done <- False
         }

      Guess ->
          let valid = validWord model.input model.words
              guess = String.toLower model.input
              guessed = List.member guess (List.map (\(x, _) -> x) model.guesses)
              result = checkGuess (String.toLower model.word) (String.toLower model.input)
          in
            if valid && not guessed
            then { model |
                           guess <- guess,
                           result <- result,
                           input <- "",
                           count <- model.count + 1,
                           done <- model.input == model.word,
                           message <- GuessMessage,
                           guesses <- model.guesses ++ [(guess, result)]
                 }
            else if guessed then
                     { model |
                           input <- "",
                           message <- GuessedMessage
                     }
            else { model |
                           input <- "",
                           message <- ErrorMessage
                 }

      UpdateWords words ->
          { emptyModel |
                         words <- words
          }

      NoWordList _ ->
          { emptyModel |
                         message <- NoWordListMessage
          }

      ShowWord ->
          {
            emptyModel |
                         cancelled <- True,
                         words <- model.words,
                         word <- model.word
          }

      PickWord seed ->
          { emptyModel |
                         word <- getRandomItem seed model.words |> Maybe.withDefault "",
                         words <- model.words,
                         message <- StartMessage
          }

      ToggleShowHistory ->
          { model |
                    show_history <- not model.show_history
          }

-- VIEW

view : Address Action -> Model -> Html
view address model =
    let
        restartButton = button [ onClick pickWord.address 1 ] [ text "Restart" ]

        showResult =
            \(b, c) -> toString b ++ " bulls, " ++ toString c ++ " cows" ++ " in '" ++ model.guess ++ "'"

        guessCount =
            \count -> toString count ++ if count == 1 then " guess" else " guesses"

        showHistoryButton = div
                          [ if (model.count > 0 && not model.done) then restartStyle else hideStyle ]
                          [ button
                            [ onClick address (if not model.show_history
                                               then ToggleShowHistory
                                               else ShowWord)
                            ]
                            [ text <| if not model.show_history then "Show history" else "Give up" ]
                          ]

        result = if (model.count > 0 && not model.done && not model.cancelled)
                 then
                     div [] [ div
                              [inputStyle]
                              [text <| if model.guess == "" then "" else showResult model.result]
                            , div [inputStyle] [guessCount model.count |> text]
                            ]
                 else div [][]

        success = "Hooray! Guessed '" ++ model.word ++ "' in " ++ guessCount model.count

        tweet_message = "Hooray! I Guessed '" ++ model.word ++ " 'in " ++ guessCount model.count ++ " at http://cowsnbulls.in"

        tweet = ( a
                  [ href ("https://twitter.com/home?status=" ++ tweet_message)
                  , target "_blank"
                  ]
                  [ img
                    [src "https://cdn3.iconfinder.com/data/icons/rcons-social/32/bird_twitter-32.png"]
                    []
                  ]
                )

        dictElements =
            [ ( a
                [ href ("http://www.dict.org/bin/Dict?Form=Dict2&Database=*&Query=" ++ model.word)
                , target "_blank"
                ]
                [ text <| "See meaning: " ++ model.word ]
              )
            , div [] [restartButton]
            ]

        doneElements = List.append
                       dictElements
                       (if model.done then [ text <| success, div[][tweet] ] else [])

        doneDiv = div [inputStyle] doneElements

        history = div [contentStyle]
                  [
                   table
                   [if model.show_history then tableStyle else hideStyle]
                   ([tr
                     []
                     [ th [] [text "Guess"]
                     , th [] [text "Bulls"]
                     , th [] [text "Cows"]
                     ]
                    ] ++
                    (List.map (\(w, (b, c)) -> tr
                                               []
                                               [ td [] [text w]
                                               , td [] [text <| toString b]
                                               , td [] [text <| toString c]
                                               ]) model.guesses))
                  ]

        siteContent = div
                      []
                      [
                       if (model.done || model.cancelled) then doneDiv else lazy2 guessWord address model
                      , result
                      , showHistoryButton
                      , history
                      ]

    in
      div
      []
      [ siteHeader
      , siteContent
      , siteFooter
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
        , placeholder <| showMessage model.message
        , maxlength 4
        , autofocus True
        , value model.input
        , name "guess"
        , on "input" targetValue (Signal.message address << UpdateInput)
        , onEnter address Guess
        , inputStyle
        ]
        []


siteFooter : Html
siteFooter = footer
             [ footerStyle ]
             [ ( a
                [ href ("https://github.com/punchagan/cowsnbulls")
                , target "_blank"
                ]
                [ text <| "Built" ]
               )
             , text " with 💔 in Bangalore. In memory of the best cows & bulls player I've known."
             ]

siteHeader : Html
siteHeader = header
             [ headerStyle ]
             [ text "Tell me you're free. I wanna play "
             , a
               [ href "https://en.wikipedia.org/wiki/Bulls_and_Cows#The_word_version"
               , target "_blank"
               ]
               [ text "Cows & Bulls!" ]
             ]



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

tableStyle : Attribute
tableStyle =
  style
    [ ("width", "100%")
    , ("font-size", "2em")
    , ("text-align", "center")
    ]

restartStyle : Attribute
restartStyle =
  style
  [ ("height", "40px")
  , ("padding", "10px")
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
    , ("height", "20px")
    , ("border-top", "solid gray 1px")
    ]

contentStyle : Attribute
contentStyle =
    style
    [
      ("bottom", "20px")
    , ("overflow", "auto")
    ]

headerStyle : Attribute
headerStyle =
  style
    [ ("width", "100%")
    , ("padding", "10px 0")
    , ("font-size", "2.25em")
    , ("opacity", "0.6")
    , ("text-align", "center")
    , ("top", "0px")
    ]

-- SIGNALS

main : Signal Html
main =
  Signal.map (view actions.address) model

-- manage the model of our application over time
model : Signal Model
model =
  Signal.foldp update emptyModel updateEvents

wordDb : Signal.Mailbox (List String)
wordDb =
    Signal.mailbox []

clockSeed : Signal Random.Seed
clockSeed = (\time -> Random.initialSeed (round time)) <~ (every millisecond)

pickWord : Signal.Mailbox Int
pickWord =
    Signal.mailbox 1

noWordList : Signal.Mailbox Bool
noWordList =
    Signal.mailbox False

updateWords : (Maybe String) -> Task String ()
updateWords content =
    case content of
      Maybe.Nothing ->
          toMaybe (getItem "words" <| Json.list Json.string)
          `andThen` setWords

      Maybe.Just s ->
          let words = (s |> String.trim |> String.lines)
          in
            (setWords <| Maybe.Just words)
            `andThen` \_ -> (setItem "words" <| Encode.list <| List.map Encode.string words)

setWords : (Maybe (List String)) -> Task String ()
setWords words =
    case words of
      Maybe.Nothing ->
          Signal.send noWordList.address True

      Maybe.Just w ->
          Signal.send wordDb.address w
               `andThen` \_ -> (Signal.send pickWord.address 1)

port getWords : Task String ()
port getWords =
    let
        url = "word_list.txt"
    in
      toMaybe (Http.getString url)
              `andThen` updateWords

-- actions from user input
actions : Signal.Mailbox Action
actions =
    Signal.mailbox NoOp

updateEvents : Signal Action
updateEvents =
    Signal.mergeMany [
               actions.signal,
               Signal.map UpdateWords wordDb.signal,
               Signal.map PickWord (Signal.sampleOn pickWord.signal clockSeed),
               Signal.map NoWordList noWordList.signal
              ]

-- HELPERS

checkGuess : String -> String -> (Int, Int)
checkGuess word guess =
    let guess_set = String.toList guess |> Set.fromList
        word_set = String.toList word |> Set.fromList
        total = Set.intersect word_set guess_set |> Set.toList |> List.length
        bulls = String.filter (\x -> let y = String.fromChar x in String.indexes y word == String.indexes y guess) guess |> String.length
    in (bulls, total - bulls)

validWord : String -> (List String) -> Bool
validWord guess words =
    List.member (String.toLower guess) words

getRandomItem : Random.Seed -> List a -> Maybe a
getRandomItem seed xs =
    let n = List.length xs
        (m, _) = Random.generate (Random.int 0 n) seed
    in
      List.head <| List.drop m xs
