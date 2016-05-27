module CowsNBulls where

import Html exposing (a, Attribute, button, div, footer, form, h3, h4, Html, input, img, p, text, table, tr, td, th, thead, tbody)
import Html.Attributes as Attr
        exposing (action, autofocus, autocomplete, class, href, id, maxlength, name, placeholder, src, target, value, style)
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
            model |
                    cancelled <- True,
                    count <- 0
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

siteFooter : Html
siteFooter = footer
             [ class "navbar navbar-default navbar-fixed-bottom"
             , style [("text-align", "center")]
             ]
             [ ( a
                [ href ("https://github.com/punchagan/cowsnbulls")
                , target "_blank"
                ]
                [ text <| "Built" ]
               )
             , text " with 💔 in Bangalore. In memory of the best "
             , ( a
                [ href ("https://en.wikipedia.org/wiki/Bulls_and_Cows#The_word_version")
                , target "_blank"
                ]
                [ text <| "cows & bulls" ]
               )
             , text " player I've known."
             ]

viewCard : List Html -> Html
viewCard contents =
    div
    [ class "row" ]
    [ div
      [ class "col-xs-6 col-xs-offset-3" ]
      [ div [ class "panel panel-default" ] contents ]
    ]

viewButton : Address Action -> Action -> String -> String -> Html
viewButton address action extraTag label = button
                                           [ onClick address action
                                           , class <| "btn pull-right " ++ extraTag
                                           ]
                                           [ text label ]

viewInputCard : Address Action -> Model -> Html
viewInputCard address model =
    let
        title = h3
                [ class "panel-title pull-left" ]
                [ text "Tell me you're free. I wanna play Cows & Bulls!" ]

        showHistoryButton = viewButton address ToggleShowHistory "btn-warning" "Show Guesses"

        giveUpButton = viewButton address ShowWord "btn-danger" "Give Up"

        restartButton = button
                        [ onClick pickWord.address 1
                        , class "btn pull-right btn-primary"
                        ]
                        [ text "Restart" ]

        scoreButton = if model.done || model.cancelled
                      then restartButton
                      else
                          if not model.show_history
                          then showHistoryButton
                          else giveUpButton

        inputHeading = div
                       [ class "panel-heading clearfix" ]
                       ( if model.done || model.cancelled || model.count > 0
                         then [ title, scoreButton ]
                         else [ title ]
                       )

        inputGuessField = input
                     [ id "guess"
                     , class "form-control"
                     , placeholder <| showMessage model.message
                     , maxlength 4
                     , autofocus True
                     , autocomplete False
                     , value model.input
                     , name "guess"
                     , on "input" targetValue (Signal.message address << UpdateInput)
                     , onEnter address Guess
                     ]
                     []

        inputGuess = div [ class "form-group" ] [ div [ class "col-xs-12" ] [ inputGuessField ] ]

        guessForm = div [ class "form-horizontal" ] [ inputGuess ]

        lastScore = \(b, c) -> toString b ++ " bulls, " ++ toString c ++ " cows"

        score = div
                [ class "list-group-item" ]
                [ p [class "list-group-item-text"] [text model.guess]
                , h4 [class "list-group-item-heading"] [text <| lastScore model.result]
                ]

        scoreBody = div [ class "list-group" ] [ score, guesses ]

        guesses = div
                  [ class "list-group-item" ]
                  [ p [class "list-group-item-text"] [text "Guesses"]
                  , h4 [class "list-group-item-heading"] [text <| toString model.count]
                  ]

        inputBody = div [ class "panel-body" ] [ guessForm ]

        dictLookup = p
                     []
                     [ text "See meaning: "
                     , a
                       [ href ("http://www.dict.org/bin/Dict?Form=Dict2&Database=*&Query=" ++ model.word)
                       , target "_blank"
                       ]
                       [ text <| model.word ]
                     ]

        guessCount =
            \count -> toString count ++ if count == 1 then " guess" else " guesses"

        successMessage = "Hooray! Guessed '" ++ model.word ++ "' in " ++ guessCount model.count

        tweetMessage = "Hooray! I Guessed '" ++ model.word ++ " 'in " ++ guessCount model.count ++ " at http://cowsnbulls.in"

        tweetLink = a
                [ href ("https://twitter.com/home?status=" ++ tweetMessage)
                , target "_blank"
                ]
                [ img
                  [src "https://cdn3.iconfinder.com/data/icons/rcons-social/32/bird_twitter-32.png"]
                  []
                ]

        success = div [] [ p [] [ text successMessage ], p [] [ tweetLink ] ]

        doneBody = div [ class "panel-body" ]
                   (if model.done then [ success, dictLookup ] else [ dictLookup ])

    in

      viewCard <|
                   [ inputHeading ]
                   ++ [ if model.done || model.cancelled then doneBody else inputBody ]
                   ++ ( if model.count > 0 then [ scoreBody ] else [] )

viewHistoryCard : Address Action -> Model -> Html
viewHistoryCard address model =
    let
        title = h3 [ class "panel-title pull-left" ] [ text "Your guesses" ]

        historyHeading = div [ class "panel-heading clearfix" ] [ title ]

        historyTable = table [ class "table table-hover" ] [ historyTableHead, historyTableBody ]

        historyTableHead = thead
                           []
                           [
                            tr
                            [ class "active" ]
                            [ th [] [text "Guess"]
                            , th [] [text "Bulls"]
                            , th [] [text "Cows"]
                            ]
                           ]

        historyTableRow =
            \(w, (b, c)) ->
               tr
               []
               [ td [] [text w]
               , td [] [text <| toString b]
               , td [] [text <| toString c]
               ]

        historyTableBody = tbody
                           []
                           (List.map historyTableRow model.guesses)

        historyBody = div [ class "table-responsive" ] [ historyTable ]

    in

      viewCard [ historyHeading, historyBody ]

view : Address Action -> Model -> Html
view address model =
    div
    []
    [ viewInputCard address model
    , if model.show_history then viewHistoryCard address model else div [] []
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
