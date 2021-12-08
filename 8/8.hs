import Data.List (elem)
import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as Set

{-
0: abcefg  (6)
1: cf      (2) #easy
2: acdeg   (5)
3: acdfg   (5)
4: bcdf    (4) #easy
5: abdfg   (5)
6: abdefg  (6)
7: acf     (3) #easy
8: abcdefg (7) #easy
9: abcdfg  (6)
-}

data Digit = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9
data Segment = A | B | C | D | E | F | G deriving (Eq, Ord)
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

main :: IO ()
main = do
    raw <- getContents
    print . countEasyDigits . parseInput $ raw