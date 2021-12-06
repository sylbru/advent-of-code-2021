import Data.Char (isSpace)
import Data.List (unfoldr)

type LanternfishAges = [Int]
type OptimizedLanternfishAges = Int

testInput :: String
testInput = "3,4,3,1,2\n"

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
parseInput =
    map read . split ',' . trim

parseInputOptimized :: String -> OptimizedLanternfishAges
parseInputOptimized =
    foldr (\acc i -> acc + 8 * i) 0 . reverse . parseInput
  
nextDay :: LanternfishAges -> LanternfishAges
nextDay ages =
    let
        births = length . filter (== 0) $ ages
    in
    (replicate births 8) ++ (map nextDaySingle ages)

nextDayOptimized :: OptimizedLanternfishAges -> OptimizedLanternfishAges
nextDayOptimized ages =
    -- TODO
    -- nextDayOptimized 14538 = 9857
    ages

nextDaySingle :: Int -> Int
nextDaySingle age =
    if age < 1 then
        6
    else
        age - 1

atDay :: Int -> LanternfishAges -> LanternfishAges
atDay d ages =
    if d <= 0 then
        ages
    else
        atDay (d - 1) (nextDay ages)

optimizedAgesToList :: OptimizedLanternfishAges -> [Int]
optimizedAgesToList ages =
    reverse (unfoldr f ages)
    where
        f :: Int -> Maybe (Int, Int)
        f v =
            case (v `div` 8, v `mod` 8) of
                (0, 0) ->
                    Nothing

                (_, remainder) ->
                    Just (remainder, (v - remainder) `div` 8)

countFish :: OptimizedLanternfishAges -> Int
countFish ages =
    if ages <= 0 then
        0
    else
        1 + (countFish $ ages `div` 8)


main :: IO ()
main = do
    raw <- getContents
    print . length . atDay 80 . parseInput $ raw
