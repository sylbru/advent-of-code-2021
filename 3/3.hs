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
    case partition ((==) One) bits of
        (trues, falses) ->
            if length trues > length falses then
                One
            else
                Zero


leastCommonValue :: [Bit] -> Bit
leastCommonValue bits =
    case partition ((==) One) bits of
        (trues, falses) ->
            if length trues <= length falses then
                One
            else
                Zero
    

toDecimal :: [Bit] -> Int
toDecimal x =
    go (length x - 1) 0 x
    where
        go :: Int -> Int -> [Bit] -> Int
        go col acc val =
            case val of
                [] -> acc
                One : xs -> go (col - 1) (acc + 2 ^ col) xs 
                Zero : xs -> go (col - 1) acc xs 


powerConsumption input = 
    gammaRate input * epsilonRate input

main :: IO ()
main = do
    raw <- getContents
    print . powerConsumption . parseInput $ raw