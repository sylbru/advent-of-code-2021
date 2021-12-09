import Data.Maybe (fromJust)
import Data.List (elem)
import Data.Foldable (find)
import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as Set

data Digit = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9 deriving (Show)
data Segment = A | B | C | D | E | F | G deriving (Show, Eq, Ord)
type DisplayedDigit = Set Segment
type Entry = ([DisplayedDigit], [DisplayedDigit])

parseInput :: String -> [Entry]
parseInput input =
    map parseEntry (lines input)

parseEntry :: String -> Entry
parseEntry input =
    let
        (rawSignalPatterns, rawOutputValue) = listTo2Tuple (splitOn " | " input)
    in
    (parseDigits rawSignalPatterns, parseDigits rawOutputValue)

parseDigits :: String -> [DisplayedDigit]
parseDigits =
    map parseDigit . words

parseDigit :: String -> DisplayedDigit
parseDigit =
    Set.fromList . map parseSegment

parseSegment :: Char -> Segment
parseSegment char =
    case char of
        'a' -> A
        'b' -> B
        'c' -> C
        'd' -> D
        'e' -> E
        'f' -> F
        'g' -> G

listTo2Tuple :: [a] -> (a, a)
listTo2Tuple (a1:a2:_) = (a1, a2)

segmentsForDigit :: Digit -> DisplayedDigit
segmentsForDigit digit =
    Set.fromList $
        case digit of
            D0 -> [A, B, C, E, F, G]
            D1 -> [C, F]
            D2 -> [A, C, D, E, G]
            D3 -> [A, C, D, F, G]
            D4 -> [B, C, D, F]
            D5 -> [A, B, D, F, G]
            D6 -> [A, B, D, E, F, G]
            D7 -> [A, C, F]
            D8 -> [A, B, C, D, E, F, G]
            D9 -> [A, B, C, D, F, G]

-- Part one

countEasyDigits :: [Entry] -> Int
countEasyDigits = sum . map countEasyDigitsForEntry

countEasyDigitsForEntry :: Entry -> Int
countEasyDigitsForEntry (_, outputValue) =
    length $ filter isEasyDigit outputValue

isEasyDigit :: DisplayedDigit -> Bool
isEasyDigit displayedDigit =
    elem (length displayedDigit) easyDigitsLengths

easyDigitsLengths :: [Int]
easyDigitsLengths =
    (map (Set.size . segmentsForDigit) [D1, D4, D7, D8])

-- Part two

type Mapping = [(Digit, DigitMapping)]
type DigitMapping = [(Segment, Set Segment)]

findMapping :: [DisplayedDigit] -> [(DisplayedDigit, Digit)]
findMapping signalPatterns =
    let
        correctWireMapping = fromJust . find (isCorrectMapping signalPatterns) $ permutations allSegments
    in
    zip
        (map ((corrupt correctWireMapping) . segmentsForDigit) allDigits)
        allDigits

isCorrectMapping :: [DisplayedDigit] -> [Segment] -> Bool
isCorrectMapping signalPatterns mapping =
    Set.fromList (map (decrypt mapping) signalPatterns)
        == Set.fromList (map segmentsForDigit allDigits)

decrypt :: [Segment] -> DisplayedDigit -> DisplayedDigit
decrypt mapping wrongDigit =
    let
        mappingTuples = zip mapping [A,B,C,D,E,F,G]
    in
    Set.map (\s -> fromJust $ lookup s mappingTuples) wrongDigit

corrupt :: [Segment] -> DisplayedDigit -> DisplayedDigit
corrupt mapping digit =
    let
        mappingTuples = zip [A,B,C,D,E,F,G] mapping
    in
    Set.map (\s -> fromJust $ lookup s mappingTuples) digit

allDigits :: [Digit]
allDigits =
    [D0, D1, D2, D3, D4, D5, D6, D7, D8, D9]

allSegments :: Set Segment
allSegments =
    Set.fromList [A, B, C, D, E, F, G]

digitToString :: Digit -> String
digitToString digit =
    case digit of
        D0 -> "0"
        D1 -> "1"
        D2 -> "2"
        D3 -> "3"
        D4 -> "4"
        D5 -> "5"
        D6 -> "6"
        D7 -> "7"
        D8 -> "8"
        D9 -> "9"

permutations :: Ord a => Set a -> [[a]]
permutations values =
    if Set.null values then
        [[]]
    else
        concat . Set.toList . (Set.map (\(first,others) -> map (first :) (permutations others)))
            $ Set.map (\v -> (v, Set.delete v values)) values

decodeOutputValue :: Entry -> Int
decodeOutputValue (signalPatterns, outputValue) =
    read . concat . map digitToString . map (decodeDigit mapping) $ outputValue
    where
        mapping = findMapping signalPatterns

decodeDigit :: [(DisplayedDigit, Digit)] -> DisplayedDigit -> Digit
decodeDigit mapping displayedDigit =
    fromJust $ lookup displayedDigit mapping

decodeOutputValues :: [Entry] -> [Int]
decodeOutputValues entries =
    map decodeOutputValue entries

sumOutputValues :: [Entry] -> Int
sumOutputValues entries =
    sum . decodeOutputValues $ entries

main :: IO ()
main = do
    raw <- getContents
    print . sumOutputValues . parseInput $ raw