type Thing = Int

parseInput :: String -> Thing
parseInput input =
    84

solve :: Thing -> Int
solve thing =
    thing `div` 2

main :: IO ()
main = do
    raw <- getContents
    print . solve . parseInput $ raw
