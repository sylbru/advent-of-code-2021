import Data.Array

type EnergyLevel = Int
type Cave = Array Int (Array Int EnergyLevel)

-- example as a single line: 5483143223\n2745854711\n5264556173\n6141336146\n6357385478\n4167524645\n2176841721\n6882881134\n4846848554\n5283751526\n

parseInput :: String -> Cave
parseInput =
    listArray (0,9) . map parseLine . lines
    where
        parseLine :: String -> Array Int EnergyLevel
        parseLine =
            listArray (0,9) . map (read . stringFromChar)

        stringFromChar :: Char -> String
        stringFromChar = (:[])


main :: IO ()
main = do
    raw <- getContents
    print . parseInput $ raw