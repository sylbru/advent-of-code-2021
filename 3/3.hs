import Data.List

data Bit = Zero | One
    deriving Eq

testInput :: String
testInput = "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010"

parseInput :: String -> [[Bit]]
parseInput input =
    map parseLine (lines input)
    where
        parseLine :: String -> [Bit]
        parseLine line = 
            map parseBit line

        parseBit :: Char -> Bit
        parseBit char =
            case char of
                '1' -> One
                _ -> Zero

gammaRate :: [[Bit]] -> Int
gammaRate =
    toDecimal . map mostCommonValue . transpose

epsilonRate :: [[Bit]] -> Int
epsilonRate =
    toDecimal . map leastCommonValue . transpose

mostCommonValue :: [Bit] -> Bit
mostCommonValue bits =
    if length ones >= length zeroes then
        One
    else
        Zero
    where
        (ones, zeroes) = partition (== One) bits


leastCommonValue :: [Bit] -> Bit
leastCommonValue bits =
    if length ones < length zeroes then
        One
    else
        Zero
    where
        (ones, zeroes) = partition (== One) bits
    

toDecimal :: [Bit] -> Int
toDecimal x =
    go (length x - 1) 0 x
    where
        go :: Int -> Int -> [Bit] -> Int
        go col acc val =
            case val of
                One : xs -> go (col - 1) (acc + 2 ^ col) xs 
                Zero : xs -> go (col - 1) acc xs 
                [] -> acc

powerConsumption :: [[Bit]] -> Int
powerConsumption report = 
    gammaRate report * epsilonRate report

-- Part 2

valueAt :: Int -> [a] -> a
valueAt i =
    head . drop i 

generalCriteria :: Int -> Bit -> [Bit] -> Bool
generalCriteria digit value number =
    valueAt digit number == value

valueForOxygenGeneratorBitCriteria :: Int -> [[Bit]] -> Bit
valueForOxygenGeneratorBitCriteria digit report =
    mostCommonValue . valueAt digit . transpose $ report

valueForCo2ScrubberBitCriteria :: Int -> [[Bit]] -> Bit
valueForCo2ScrubberBitCriteria digit report =
    leastCommonValue . valueAt digit . transpose $ report

rating :: (Int -> [[Bit]] -> Bit) -> [[Bit]] -> Int
rating valueForCriteria report =
    go 0 valueForCriteria report
    where
        go :: Int -> (Int -> [[Bit]] -> Bit) -> [[Bit]] -> Int
        go digit valueForCriteria_ report_ =
            case filter (generalCriteria digit (valueForCriteria_ digit report_)) report_ of
                [] -> 0
                [number] -> toDecimal number
                matchingNumbers -> go (digit + 1) valueForCriteria_ matchingNumbers


oxygenGeneratorRating :: [[Bit]] -> Int
oxygenGeneratorRating report =
    rating valueForOxygenGeneratorBitCriteria report

co2ScrubberRating :: [[Bit]] -> Int
co2ScrubberRating report =
    rating valueForCo2ScrubberBitCriteria report

lifeSupportRating :: [[Bit]] -> Int
lifeSupportRating report =
    oxygenGeneratorRating report * co2ScrubberRating report

main :: IO ()
main = do
    raw <- getContents
    -- print . powerConsumption . parseInput $ raw
    print . lifeSupportRating . parseInput $ raw