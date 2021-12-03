prepareInput :: String -> [Int]
prepareInput =
    map read . words

countIncreases :: Ord a => [a] -> Int
countIncreases list =
    case list of
        x : x' : xs ->
            countIncreases (x' : xs) + (if x' > x then 1 else 0)

        _ -> 0


main :: IO ()
main = do
    raw <- getContents
    print . countIncreases . prepareInput $ raw
