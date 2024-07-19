from random import randint

bibleDict = {1 : "Genesis"
	     , 2 : "Exodus"
	     , 3 : "Leviticus"
    	     , 4 : "Numbers"
    	     , 5 : "Deuteronomy"
    	     , 6 : "Joshua"
	     , 7 : "Judges"
	     , 8 : "Ruth"
	     , 9 : "OneSamuel"
	     , 10 : "TwoSamuel"
	     , 11 : "OneKings"
	     , 12 : "TwoKings"
	     , 13 : "OneChronicles"
	     , 14 : "TwoChronicles"
	     , 15 : "Ezra"
	     , 16 : "Nehemiah"
	     , 17 : "Esther"
	     , 18 : "Job"
	     , 19 : "Psalms"
	     , 20 : "Proverbs"
	     , 21 : "Ecclesiastes"
	     , 22 : "SongOffSolomon"
	     , 23 : "Isaiah"
	     , 24 : "Jeremiah"
	     , 25 : "Lamentations"
	     , 26 : "Ezekiel"
	     , 27 : "Daniel"
	     , 28 : "Hosea"
	     , 29 : "Joel"
	     , 30 : "Amos"
	     , 31 : "Obadiah"
	     , 32 : "Jonah"
	     , 33 : "Micah"
	     , 34 : "Nahum"
	     , 35 : "Habakkuk"
	     , 36 : "Zephaniah"
	     , 37 : "Haggai"
	     , 38 : "Zechariah"
	     , 39 : "Malachi"
	     , 40 : "Matthew"
	     , 41 : "Mark"
	     , 42 : "Luke"
	     , 43 : "John"
	     , 44 : "Acts"
	     , 45 : "Romans"
	     , 46 : "OneCorinthians"
	     , 47 : "TwoCorinthians"
	     , 48 : "Galatians"
	     , 49 : "Ephesians"
	     , 50 : "Philippians"
	     , 51 : "Colossians"
	     , 52 : "OneThessalonians"
	     , 53 : "TwoThessalonians"
	     , 54 : "OneTimothy"
	     , 55 : "TwoTimothy"
	     , 56 : "Titus"
	     , 57 : "Philemon"
	     , 58 : "Hebrews"
	     , 59 : "James"
	     , 60 : "OnePeter"
	     , 61 : "TwoPeter"
	     , 62 : "OneJohn"
	     , 63 : "TwoJohn"
	     , 64 : "ThreeJohn"
	     , 65 : "Jude"
	     , 66 : "Revelation"
	     , 0 : "None"}

bible = dict()
# read in bible
with open("t_kjv.csv") as f:
    for l in f:
        parts = l.split(",", maxsplit=4)
        
        #print(parts)
        if parts[1] not in bible: #adding book
            bible[parts[1]] = dict()
        if parts[2] not in bible[parts[1]]: #adding chp
            bible[parts[1]][parts[2]] = dict()
        if parts[4][-1] == "\n":
            parts[4] = parts[4][:-1]
        if parts[4][0] == "\"" and parts[4][-1] == "\"":
            parts[4] = parts[4][1:-1]
        parts[4] = parts[4].replace('"', '\"')
        
        bible[parts[1]][parts[2]][parts[3]] = parts[4]

LEN = 2000
#generate quotes (2000?)
toPrint = ["x"] * LEN
toFill = 0
while toFill < LEN:
    nextBook = str(randint(1, 66))
    nextChp = str(randint(1, len(bible[nextBook])))
    x = len(bible[nextBook][nextChp]) - 3
    if x <= 0:
        continue
    nextVerse = randint(1, x)
    wordsToFill = (" ".join([", (BibleVerse",
                            bibleDict[int(nextBook)],
                            nextChp,
                            str(nextVerse)])
                   + (", [\""
                      + bible[nextBook][nextChp][str(nextVerse)]
                      + "\", \""
                      + bible[nextBook][nextChp][str(nextVerse + 1)]
                      + "\", \""
                      + bible[nextBook][nextChp][str(nextVerse + 2)]
                      + "\", \""
                      + bible[nextBook][nextChp][str(nextVerse + 3)]
                      + "\"])")
                   )
    toPrint[toFill] = wordsToFill
    toFill += 1

for x in toPrint:
    print(x)

