port module Main exposing (..)

import Browser
import Date exposing (Date)
import Types exposing (..)
import VerseList exposing (..)
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Html exposing (Html, div, span, h1, h2, h3, button, text, p)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Tuple exposing (first, second)
import Task


main : Program () Model Msg
main =
    Browser.document
        { init =
            (\flags ->
                ( Setup
                , Date.today |> (Task.perform ReceiveDate)
                )
             --sendMessage "getPkgList"
            )
        , update = update
        , view = view
        , subscriptions = subs
        }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case model of
        Setup ->
            (updateSetup msg, Cmd.none)

        Playing mod ->
            updatePlaying msg mod

        Info mod -> (mod, Cmd.none)

        Finished mod -> updateFinished msg mod

updateSetup : Msg -> Model
updateSetup msg =
    case msg of
        ReceiveDate d ->
            let
                realAnswer : (BibleVerse, List String)
                realAnswer = getAns d

                ans : BibleBook
                ans = (first realAnswer).book
            in
                Playing (ModelPlaying (first realAnswer)
                             ans
                             (second realAnswer)
                             []
                             None
                             Dropdown.initialState
                             (Date.diff Date.Days epoch d)
                        )

        _ -> Setup

getAns : Date -> (BibleVerse, List String)
getAns d =
    let
        getAnsH : Int -> List (BibleVerse, List String) -> (BibleVerse, List String)
        getAnsH i l =
            case l of
                head :: tail -> if i == 0 then head else getAnsH (i - 1) tail
                [] -> (BibleVerse None 0 0, ["ERROR - OUT OF QUOTES, verselist has " ++ String.fromInt (List.length verseList) ++ "and n is" ++ String.fromInt(Date.diff Date.Days epoch d)])
    in
        getAnsH (Date.diff Date.Days epoch d) verseList
             
updatePlaying : Msg -> ModelPlaying -> (Model, Cmd Msg)
updatePlaying msg model =
    case msg of
        Guess ->
            if (model.ans == model.currGuess) || ((List.length model.guessesOld) == 3) then -- either won or lost
                (Finished (ModelDone model.realAns
                               model.ans
                               (model.currGuess :: model.guessesOld)
                               model.clues
                               (if model.ans /= model.currGuess then 10 else List.length (model.currGuess :: model.guessesOld)) -- 10 is X/4
                               model.n)
                , Cmd.none)
            else
                (Playing {model | currGuess = None, guessesOld = model.currGuess :: model.guessesOld}, Cmd.none)

        NewBook b ->
            (Playing {model | currGuess = b}, Cmd.none)

        DropMsg x ->
            (Playing {model | bookDropState = x}, Cmd.none)

        ToInfo ->
            (Info (Playing model), Cmd.none)

        _ -> (Playing model, Cmd.none)

updateFinished : Msg -> ModelDone -> (Model, Cmd Msg)
updateFinished msg model =
    case msg of
        ToInfo ->
            (Info (Finished model), Cmd.none)

        AskForCopy s ->
            (Finished model, sendMessage s)
        _ ->
            (Finished model, Cmd.none)
                     

view : Model -> Browser.Document Msg
view model =
    Browser.Document
        "Bible"
        [ div [style "margin" "20px"]
              [ h3 [] [text "Bible"]
              , CDN.stylesheet
              , case model of
                    Playing mod -> viewPlaying mod
                    Setup -> viewSetup
                    Finished mod -> viewFinished mod
                    Info _ -> viewInfo
              , p [] []
              , case model of
                    Info _ -> span [] []
                    _ ->  button [onClick ToInfo] [text "Help"]
              ]
        ]

viewPlaying : ModelPlaying -> Html Msg
viewPlaying model =
    div []
        [ div [] [text (String.join " " (List.take ((List.length model.guessesOld) + 1) model.clues))]
        , p [] []
        , renderDropdown model
        , button [ onClick Guess] [text "Guess!"]
        ]

renderDropdown : ModelPlaying -> Html Msg
renderDropdown model =
    div [] [Dropdown.dropdown model.bookDropState
                { toggleMsg = DropMsg
                , toggleButton = Dropdown.toggle [ Button.primary ] [ text (if model.currGuess == None then "Select!" else bookToStr(model.currGuess)) ]
                , options = []
                , items = [Dropdown.header [ text "OT"]]
                          ++ List.map (\ x -> Dropdown.buttonItem [ onClick (NewBook x)] [ text (bookToStr x)]) ot
                          ++ [Dropdown.header [ text "NT"]]
                          ++ List.map (\ x -> Dropdown.buttonItem [ onClick (NewBook x)] [ text (bookToStr x)]) nt
                          ++ [Dropdown.header [ text "Cancel" ]
                             , Dropdown.buttonItem [ onClick (NewBook None) ] [ text "Cancel"]
                             ]
                }
           ]
        
viewSetup : Html Msg
viewSetup =
    text "setting up"

viewFinished : ModelDone -> Html Msg
viewFinished mod =
    div []
        [ div [] [text (String.join " " mod.clues)]
        , div [] [text (bookToStr mod.realAns.book
                            ++ " "
                            ++ String.fromInt mod.realAns.chp
                            ++ ":"
                            ++ String.fromInt mod.realAns.verse
                            ++ "-"
                            ++ String.fromInt(mod.realAns.verse + 3))]
        , div [] [text "share your score!"
                 , button [onClick (AskForCopy ((renderScore mod) ++ " https://tglenhab.github.io/bible/"))]
                     [text "Share!"]]
        , div [] [text (renderScore mod)]
        , renderGuesses mod
        ]
        
renderScore : ModelDone -> String
renderScore mod =
    ("Bible #"
         ++ (String.fromInt mod.n)
         ++ " "
         ++ (if List.member mod.ans mod.guesses then (String.fromInt (List.length mod.guesses)) else "X")
         ++ "/4 "
         ++ (String.join "" (List.reverse (List.map (\ x -> if x == mod.ans then "📗" else if ((whichSection mod.ans) == (whichSection x)) then "📒" else "📕") mod.guesses)))
    )

renderGuesses : ModelDone -> Html Msg
renderGuesses mod =
    div [] (List.map (\ x -> div [style "color" (if x == mod.ans then "black" else "gray")]
                          [text (if x /= None then bookToStr(x) else "Skipped")])
                mod.guesses)


viewInfo : Html Msg
viewInfo =
    div []
        [ bibleInfoText
        , button [ onClick Back ] [ text "back" ]
        ]

bibleInfoText : Html Msg
bibleInfoText =
    div [] [ h2 [] [text "Bible: Another derivative Wordle clone"]
           , div [] [text "In Bible, try to guess the book of the Bible the verse(s) are chosen from. For each wrong guess, another verse is revealed. You have four guesses."]
           , div [] [text "Bible resets at midnight each day, on your computer's local time."]
           , div [] [text "Bible uses the King James Version of the bible as its source material, taken from https://github.com/scrollmapper/bible_databases"]
           , div [] [text "as a small note, the share feature only works on secure connections; check for https:// in the url"]
           ]
        
subs : Model -> Sub Msg
subs model =
    case model of
        Playing mod ->
            Dropdown.subscriptions mod.bookDropState DropMsg

        _ ->
            Sub.none
                
port sendMessage : String -> Cmd msg
