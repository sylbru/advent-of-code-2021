testInput :: [Int]
testInput = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]

prepareInput :: String -> [Int]
prepareInput =
    map read . words

countIncreases :: Ord a => [a] -> Int
countIncreases list =
    case list of
        x : x' : xs ->
            countIncreases (x' : xs) + (if x' > x then 1 else 0)

        _ -> 0


toSums :: [Int] -> [Int]
toSums list =
    case list of
        x : x' : x'' : xs ->
            (x + x' + x'') : toSums (x' : x'' : xs)

        _ -> []

main :: IO ()
main = do
    raw <- getContents
    print . countIncreases . toSums $ prepareInput raw
