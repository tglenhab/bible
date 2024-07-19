module Types exposing (..)

import Bootstrap.Dropdown as Dropdown
import Date exposing(Date)
import Time

type Model
    = Playing ModelPlaying
    | Setup
    | Finished ModelDone
    | Info Model


type alias ModelPlaying =
    { realAns : BibleVerse
    , ans : BibleBook
    , clues : List String
    , guessesOld : List BibleBook
    , currGuess : BibleBook
    , bookDropState : Dropdown.State
    , n : Int
    }


type alias ModelDone =
    { realAns : BibleVerse
    , ans : BibleBook
    , guesses : List BibleBook
    , clues : List String
    , numGuesses : Int
    , n : Int
    }

type alias BibleVerse =
    { book : BibleBook
    , chp : Int
    , verse : Int
    }
    

type Msg
    = Guess
    | NewBook BibleBook
    | ReceiveDate Date
    | DropMsg Dropdown.State
    | Back
    | ToInfo
    | AskForCopy String


type BibleBook
    = Genesis
    | Exodus
    | Leviticus
    | Numbers
    | Deuteronomy
    | Joshua
    | Judges
    | Ruth
    | OneSamuel
    | TwoSamuel
    | OneKings
    | TwoKings
    | OneChronicles
    | TwoChronicles
    | Ezra
    | Nehemiah
    | Esther
    | Job
    | Psalms
    | Proverbs
    | Ecclesiastes
    | SongOffSolomon
    | Isaiah
    | Jeremiah
    | Lamentations
    | Ezekiel
    | Daniel
    | Hosea
    | Joel
    | Amos
    | Obadiah
    | Jonah
    | Micah
    | Nahum
    | Habakkuk
    | Zephaniah
    | Haggai
    | Zechariah
    | Malachi
    | Matthew
    | Mark
    | Luke
    | John
    | Acts
    | Romans
    | OneCorinthians
    | TwoCorinthians
    | Galatians
    | Ephesians
    | Philippians
    | Colossians
    | OneThessalonians
    | TwoThessalonians
    | OneTimothy
    | TwoTimothy
    | Titus
    | Philemon
    | Hebrews
    | James
    | OnePeter
    | TwoPeter
    | OneJohn
    | TwoJohn
    | ThreeJohn
    | Jude
    | Revelation
    | None

ot : List BibleBook
ot =
    [Genesis
    , Exodus
    , Leviticus
    , Numbers
    , Deuteronomy
    , Joshua
    , Judges
    , Ruth
    , OneSamuel
    , TwoSamuel
    , OneKings
    , TwoKings
    , OneChronicles
    , TwoChronicles
    , Ezra
    , Nehemiah
    , Esther
    , Job
    , Psalms
    , Proverbs
    , Ecclesiastes
    , SongOffSolomon
    , Isaiah
    , Jeremiah
    , Lamentations
    , Ezekiel
    , Daniel
    , Hosea
    , Joel
    , Amos
    , Obadiah
    , Jonah
    , Micah
    , Nahum
    , Habakkuk
    , Zephaniah
    , Haggai
    , Zechariah
    , Malachi
    ]
      
nt : List BibleBook
nt =
    [ Matthew
    , Mark
    , Luke
    , John
    , Acts
    , Romans
    , OneCorinthians
    , TwoCorinthians
    , Galatians
    , Ephesians
    , Philippians
    , Colossians
    , OneThessalonians
    , TwoThessalonians
    , OneTimothy
    , TwoTimothy
    , Titus
    , Philemon
    , Hebrews
    , James
    , OnePeter
    , TwoPeter
    , OneJohn
    , TwoJohn
    , ThreeJohn
    , Jude
    , Revelation    
     ]

type BibleSection
    = Torah
    | Neviim
    | Ketuvim
    | Gospels
    | Act
    | Epistles
    | Rev
    | Other


whichSection : BibleBook -> BibleSection
whichSection b =
    if
        inL b
            [ Genesis
            , Exodus
            , Leviticus
            , Numbers
            , Deuteronomy
            ]
    then
        Torah

    else if
        inL b
            [ Joshua
            , Judges
            , Ruth
            , OneSamuel
            , TwoSamuel
            , OneKings
            , TwoKings
            , Isaiah
            , Jeremiah
            , Ezekiel
            , Hosea
            , Joel
            , Amos
            , Obadiah
            , Jonah
            , Micah
            , Nahum
            , Habakkuk
            , Zephaniah
            , Haggai
            , Zechariah
            , Malachi
            ]
    then
        Neviim

    else if
        inL b
            [ Job
            , Psalms
            , Proverbs
            , Ecclesiastes
            , SongOffSolomon
            , Ruth
            , Lamentations
            , Esther
            , OneChronicles
            , TwoChronicles
            , Ezra
            , Nehemiah
            , Daniel
            ]
    then
        Ketuvim

    else if
        inL b
            [ Matthew
            , Mark
            , Luke
            , John
            ]
    then
        Gospels

    else if
        inL b
            [ Acts
            ]
    then
        Act

    else if
        inL b
            [ Romans
            , OneCorinthians
            , TwoCorinthians
            , Galatians
            , Ephesians
            , Philippians
            , Colossians
            , OneThessalonians
            , TwoThessalonians
            , OneTimothy
            , TwoTimothy
            , Titus
            , Philemon
            , Hebrews
            , James
            , OnePeter
            , TwoPeter
            , OneJohn
            , TwoJohn
            , ThreeJohn
            , Jude
            ]
    then
        Epistles

    else if inL b [ Revelation ] then
        Rev

    else
        Other




inL : a -> List a -> Bool
inL a l =
    case l of
        [] ->
            False

        x :: y ->
            (x == a) || inL a y


strToBook : String -> BibleBook
strToBook s =
    case s of
        "Genesis" ->
            Genesis

        "Exodus" ->
            Exodus

        "Leviticus" ->
            Leviticus

        "Numbers" ->
            Numbers

        "Deuteronomy" ->
            Deuteronomy

        "Joshua" ->
            Joshua

        "Judges" ->
            Judges

        "Ruth" ->
            Ruth

        "1 Samuel" ->
            OneSamuel

        "2 Samuel" ->
            TwoSamuel

        "1 Kings" ->
            OneKings

        "2 Kings" ->
            TwoKings

        "1 Chronicles" ->
            OneChronicles

        "2 Chronicles" ->
            TwoChronicles

        "Ezra" ->
            Ezra

        "Nehemiah" ->
            Nehemiah

        "Esther" ->
            Esther

        "Job" ->
            Job

        "Psalms" ->
            Psalms

        "Proverbs" ->
            Proverbs

        "Ecclesiastes" ->
            Ecclesiastes

        "Song Of Solomon" ->
            SongOffSolomon

        "Isaiah" ->
            Isaiah

        "Jeremiah" ->
            Jeremiah

        "Lamentations" ->
            Lamentations

        "Ezekiel" ->
            Ezekiel

        "Daniel" ->
            Daniel

        "Hosea" ->
            Hosea

        "Joel" ->
            Joel

        "Amos" ->
            Amos

        "Obadiah" ->
            Obadiah

        "Jonah" ->
            Jonah

        "Micah" ->
            Micah

        "Nahum" ->
            Nahum

        "Habakkuk" ->
            Habakkuk

        "Zephaniah" ->
            Zephaniah

        "Haggai" ->
            Haggai

        "Zechariah" ->
            Zechariah

        "Malachi" ->
            Malachi

        "Matthew" ->
            Matthew

        "Mark" ->
            Mark

        "Luke" ->
            Luke

        "John" ->
            John

        "Acts" ->
            Acts

        "Romans" ->
            Romans

        "1 Corinthians" ->
            OneCorinthians

        "2 Corinthians" ->
            TwoCorinthians

        "Galatians" ->
            Galatians

        "Ephesians" ->
            Ephesians

        "Philippians" ->
            Philippians

        "Colossians" ->
            Colossians

        "1 Thessalonians" ->
            OneThessalonians

        "2 Thessalonians" ->
            TwoThessalonians

        "1 Timothy" ->
            OneTimothy

        "2 Timothy" ->
            TwoTimothy

        "Titus" ->
            Titus

        "Philemon" ->
            Philemon

        "Hebrews" ->
            Hebrews

        "James" ->
            James

        "1 Peter" ->
            OnePeter

        "2 Peter" ->
            TwoPeter

        "1 John" ->
            OneJohn

        "2 John" ->
            TwoJohn

        "3 John" ->
            ThreeJohn

        "Jude" ->
            Jude

        "Revelation" ->
            Revelation

        _ ->
            None


bookToStr : BibleBook -> String
bookToStr b =
    case b of
        Genesis ->
            "Genesis"

        Exodus ->
            "Exodus"

        Leviticus ->
            "Leviticus"

        Numbers ->
            "Numbers"

        Deuteronomy ->
            "Deuteronomy"

        Joshua ->
            "Joshua"

        Judges ->
            "Judges"

        Ruth ->
            "Ruth"

        OneSamuel ->
            "1 Samuel"

        TwoSamuel ->
            "2 Samuel"

        OneKings ->
            "1 Kings"

        TwoKings ->
            "2 Kings"

        OneChronicles ->
            "1 Chronicles"

        TwoChronicles ->
            "2 Chronicles"

        Ezra ->
            "Ezra"

        Nehemiah ->
            "Nehemiah"

        Esther ->
            "Esther"

        Job ->
            "Job"

        Psalms ->
            "Psalms"

        Proverbs ->
            "Proverbs"

        Ecclesiastes ->
            "Ecclesiastes"

        SongOffSolomon ->
            "Song of Solomon"

        Isaiah ->
            "Isaiah"

        Jeremiah ->
            "Jeremiah"

        Lamentations ->
            "Lamentations"

        Ezekiel ->
            "Ezekiel"

        Daniel ->
            "Daniel"

        Hosea ->
            "Hosea"

        Joel ->
            "Joel"

        Amos ->
            "Amos"

        Obadiah ->
            "Obadiah"

        Jonah ->
            "Jonah"

        Micah ->
            "Micah"

        Nahum ->
            "Nahum"

        Habakkuk ->
            "Habakkuk"

        Zephaniah ->
            "Zephaniah"

        Haggai ->
            "Haggai"

        Zechariah ->
            "Zechariah"

        Malachi ->
            "Malachi"

        Matthew ->
            "Matthew"

        Mark ->
            "Mark"

        Luke ->
            "Luke"

        John ->
            "John"

        Acts ->
            "Acts"

        Romans ->
            "Romans"

        OneCorinthians ->
            "1 Corinthians"

        TwoCorinthians ->
            "2 Corinthians"

        Galatians ->
            "Galatians"

        Ephesians ->
            "Ephesians"

        Philippians ->
            "Philippians"

        Colossians ->
            "Colossians"

        OneThessalonians ->
            "1 Thessalonians"

        TwoThessalonians ->
            "2 Thessalonians"

        OneTimothy ->
            "1 Timothy"

        TwoTimothy ->
            "2 Timothy"

        Titus ->
            "Titus"

        Philemon ->
            "Philemon"

        Hebrews ->
            "Hebrews"

        James ->
            "James"

        OnePeter ->
            "1 Peter"

        TwoPeter ->
            "2 Peter"

        OneJohn ->
            "1 John"

        TwoJohn ->
            "2 John"

        ThreeJohn ->
            "3 John"

        Jude ->
            "Jude"

        Revelation ->
            "Revelation"

        _ ->
            ""
sanToInt : String -> Int
sanToInt s =
    Maybe.withDefault 0 (String.toInt s)

epoch : Date
epoch =
    Date.fromCalendarDate 2022 Time.Jul 26
