type EnergyLevel = Int
type Cave = [[EnergyLevel]]

-- example as a single line: 5483143223\n2745854711\n5264556173\n6141336146\n6357385478\n4167524645\n2176841721\n6882881134\n4846848554\n5283751526\n

parseInput :: String -> Cave
parseInput =
    map parseLine . lines
    where
        parseLine :: String -> [EnergyLevel]
        parseLine =
            map (read . stringFromChar)

        stringFromChar :: Char -> String
        stringFromChar = (:[])

printMap :: Cave -> String
printMap =
    unlines . map printLine
    where printLine = concat . map show

main :: IO ()
main = do
    raw <- getContents
    putStrLn . printMap . parseInput $ raw