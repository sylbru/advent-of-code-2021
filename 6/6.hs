import Data.Char (isSpace)
import Data.List (unfoldr)

type LanternfishAges = Int

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
    foldr (\acc i -> acc + 16 * i) 0 . reverse . map read . split ',' . trim


nextDay :: LanternfishAges -> LanternfishAges
nextDay ages_ =
    -- nextDay 14538 = 9857
    -- nextDay 9857 = 41864 (pas forcément : l’ordre peut être différent, donc la représentation aussi)
    -- countFish (nextDay 9857) == countFish 41864
    go 0 ages_
    where
        go col ages =
            if ages == 0 then
                0
            else
                if ages `mod` 16 > 0 then
                    (go (col + 1) (ages `div` 16))
                        + (16 ^ col) * (ages `mod` 16 - 1) 
                else
                    (go (col + 2) (ages * 16))
                    + 104 -- (6 et 8)

atDay :: Int -> LanternfishAges -> LanternfishAges
atDay d ages =
    if d <= 0 then
        ages
    else
        atDay (d - 1) (nextDay ages)

agesToList :: LanternfishAges -> [Int]
agesToList ages =
    reverse (unfoldr f ages)
    where
        f :: Int -> Maybe (Int, Int)
        f v =
            case (v `div` 16, v `mod` 16) of
                (0, 0) ->
                    Nothing

                (_, remainder) ->
                    Just (remainder, (v - remainder) `div` 16)

countFish :: LanternfishAges -> Int
countFish ages_ =
    go 0 ages_
    where
        go :: Int -> LanternfishAges -> Int
        go acc ages =
            if ages <= 0 then
                acc
            else
                go (acc + 1) (ages `div` 16)


main :: IO ()
main = do
    raw <- getContents
    print . countFish . atDay 80 . parseInput $ testInput
