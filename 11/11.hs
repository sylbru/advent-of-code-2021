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

step :: Cave -> Cave
step =
    resetFlashing . flash . incrementAll
    where
        incrementAll :: Cave -> Cave
        incrementAll =
            map (map (\energy -> if energy >= 0 then energy + 1 else energy))

        flash :: Cave -> Cave
        flash =
            map (map (\energy -> if energy > 9 then -1 else energy))

        resetFlashing :: Cave -> Cave
        resetFlashing =
            map (map (\energy -> if energy < 0 then 0 else energy))

flash :: Cave -> Cave
flash cave =
    cave

withCoords :: Cave -> [[((Int, Int), EnergyLevel)]]
withCoords cave =
    indexedMap
        (\(col, line) ->
            indexedMap
                (\(row, energyLevel) ->
                    ((col, row), energyLevel)
                )
                line
        )
        cave


indexedMap :: ((Int, a) -> b) -> [a] -> [b]
indexedMap f l =
    go 0 f l
    where
        go :: Int -> ((Int, a) -> b) -> [a] -> [b]
        go _ _ [] = []
        go i fun (x:xs) =
            fun (i,x) : go (i + 1) fun xs

printMap :: Cave -> String
printMap =
    unlines . map printLine
    where printLine = concat . map show

main :: IO ()
main = do
    raw <- getContents
    putStrLn . printMap . parseInput $ raw
    putStrLn . printMap . step . parseInput $ raw
    putStrLn . printMap . step . step . parseInput $ raw