type EnergyLevel = Int
type Cave = [[EnergyLevel]]

parseInput :: String -> Cave
parseInput =
    map parseLine . lines
    where
        parseLine :: String -> [EnergyLevel]
        parseLine =
            map (read . stringFromChar)

        stringFromChar :: Char -> String
        stringFromChar = (:[])

stepsToSynchronize :: Cave -> Int
stepsToSynchronize cave_ =
    go 0 cave_
    where
        go :: Int -> Cave -> Int
        go acc cave =
            if all (all (== 0)) cave then
                acc
            else
                go (acc + 1) (fst $ step cave)


flashesForNSteps :: Int -> Cave -> Int
flashesForNSteps n_ cave_ =
    go 0 n_ cave_
    where
        go :: Int -> Int -> Cave -> Int
        go acc n cave
            | n <= 0 = acc
            | otherwise =
                let
                    (newCave, flashes) = step cave
                in
                go (acc + flashes) (n - 1) newCave

step :: Cave -> (Cave, Int)
step cave =
    let
        newCaveBeforeReset = flash . prepareFlashing . incrementAll $ cave
    in
    resetFlashing newCaveBeforeReset

incrementAll :: Cave -> Cave
incrementAll =
    map (map (\energy -> if energy >= 0 then energy + 1 else energy))

prepareFlashing :: Cave -> Cave
prepareFlashing =
    map (map (\energy -> if energy > 9 then -1 else energy))

resetFlashing :: Cave -> (Cave, Int)
resetFlashing cave =
    (map (map (\energy -> if energy < 0 then 0 else energy)) cave
    , length . filter (\energy -> energy < 0) . concat $ cave
    )

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
    print . flashesForNSteps 100 . parseInput $ raw
    print . stepsToSynchronize . parseInput $ raw
