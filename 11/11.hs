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


nSteps :: Int -> Cave -> Cave
nSteps n cave
    | n <= 0 = cave
    | otherwise = nSteps (n - 1) (step cave)


step :: Cave -> Cave
step =
    resetFlashing . flash . prepareFlashing . incrementAll
    where
        incrementAll :: Cave -> Cave
        incrementAll =
            map (map (\energy -> if energy >= 0 then energy + 1 else energy))


prepareFlashing :: Cave -> Cave
prepareFlashing =
    map (map (\energy -> if energy > 9 then -1 else energy))

resetFlashing :: Cave -> Cave
resetFlashing =
    map (map (\energy -> if energy < 0 then 0 else energy))

flashingDone :: Cave -> Cave
flashingDone =
    map (map (\energy -> if energy < 0 then energy - 1 else energy))

flash :: Cave -> Cave
flash cave =
    let
        flashing =
            map fst $ filter (\(_, energyLevel) -> energyLevel == -1) (concat $ withCoords cave)
    in
    if null flashing then
        cave
    else
        flash . prepareFlashing . flashingDone . applyFlashes flashing $ cave

applyFlashes :: [(Int, Int)] -> Cave -> Cave
applyFlashes [] cave = cave
applyFlashes (x:xs) cave =
    let
        newCave = incrementSeveralAt (adjacentPoints x) cave
    in
    applyFlashes xs newCave

adjacentPoints :: (Int, Int) -> [(Int, Int)]
adjacentPoints (col, row) =
    [ (y, x) |
        y <- [col - 1..col + 1],
        x <- [row - 1..row + 1],
        (y, x) /= (col, row)
    ]

incrementSeveralAt :: [(Int, Int)] -> Cave -> Cave
incrementSeveralAt coords cave =
    indexedMap
        (\(col, line) ->
            indexedMap
                (\(row, energyLevel) ->
                    if energyLevel >= 0 && (col, row) `elem` coords then
                        energyLevel + 1
                    else
                        energyLevel
                )
                line
        )
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
    putStrLn . printMap . nSteps 100 . parseInput $ raw
