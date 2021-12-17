import Data.Char (isSpace)
import Data.List (unfoldr)
import Data.Map.Strict (Map, fromList, (!), adjust)

-- Map: age -> count
type LanternfishAges = Map Int Int

trim :: String -> String
trim = trimWith isSpace

trimWith :: (Char -> Bool) -> String -> String
trimWith predicate = f . f
    where f = reverse . dropWhile predicate

split :: Char -> String -> [String]
split delimiter string =
    unfoldr (splitter delimiter) string
    where
        splitter :: Char -> String -> Maybe (String, String)
        splitter delim s =
            case span (/= delim) s of
                ("", "") ->
                    Nothing
                ("", rest) ->
                    Just (drop 1 rest, "")
                (item, rest) ->
                    Just (item, drop 1 rest)


parseInput :: String -> LanternfishAges
parseInput input =
    go (fromList $ map (\age -> (age, 0)) [0..8]) (map read . split ',' . trim $ input)
    where
        go :: LanternfishAges -> [Int] -> LanternfishAges
        go ages [] = ages
        go ages (fishAge:rest) =
            go (adjust (+ 1) fishAge ages) rest


countFishAtDay :: Int -> LanternfishAges -> Int
countFishAtDay 0 ages =
    foldr (+) 0 ages
countFishAtDay n ages =
    countFishAtDay (n - 1) (step ages)

step :: LanternfishAges -> LanternfishAges
step ages =
    fromList
        [ (0, ages ! 1)
        , (1, ages ! 2)
        , (2, ages ! 3)
        , (3, ages ! 4)
        , (4, ages ! 5)
        , (5, ages ! 6)
        , (6, (ages ! 7) + (ages ! 0))
        , (7, ages ! 8)
        , (8, ages ! 0)
        ]

main :: IO ()
main = do
    raw <- getContents
    print . countFishAtDay 256 . parseInput $ raw
