import Data.List.Split (splitOn)
import Data.List (nub)

data Axis = X | Y

parse :: String -> ([(Int, Int)], [(Axis, Int)])
parse input =
    case splitOn "\n\n" input of
        dots : instructions : [] ->
            (parseDots dots, parseInstructions instructions)

parseDots :: String -> [(Int, Int)]
parseDots input =
    map (listTo2Tuple . map read . take 2 . splitOn ",") . lines $ input

parseInstructions :: String -> [(Axis, Int)]
parseInstructions input =
    map parseInstruction (lines input)
    where
        parseInstruction line =
            case words line of
                _:_:(axis:'=':value):[] -> (if axis == 'x' then X else Y, read value)

listTo2Tuple :: [a] -> (a, a)
listTo2Tuple (a1:a2:_) = (a1, a2)

foldY :: Int -> [(Int, Int)] -> [(Int, Int)]
foldY yAxis sheet =
    nub $ map (\(x,y) -> if y > yAxis then (x, yAxis * 2 - y) else (x, y)) sheet

foldX :: Int -> [(Int, Int)] -> [(Int, Int)]
foldX xAxis sheet =
    nub $ map (\(x,y) -> if x > xAxis then (xAxis * 2 - x, y) else (x, y)) sheet

foldAll :: ([(Int, Int)], [(Axis, Int)]) -> [(Int, Int)]
foldAll (dots, []) = dots
foldAll (dots, instruction:xs) =
    case instruction of
        (X, value) -> foldAll ((foldX value dots), xs)
        (Y, value) -> foldAll ((foldY value dots), xs)

printDots :: [(Int, Int)] -> String
printDots dots =
    let
        maxX = maximum . map fst $ dots
        maxY = maximum . map snd $ dots
        emptyImage = (replicate maxY (replicate maxX False))
    in
    unlines . map (map (\dot -> if dot then '#' else '.'))
        $ go dots emptyImage
    where
        go :: [(Int, Int)] -> [[Bool]] -> [[Bool]]
        go [] image = image
        go ((x,y):rest) image =
            go rest $
                indexedMap
                    (\(y_,row) ->
                        if y_ == y then
                            indexedMap
                                (\(x_,val) ->
                                    if x == x_ then True else val
                                )
                                row
                        else
                            row
                    )
                    image



indexedMap :: ((Int, a) -> b) -> [a] -> [b]
indexedMap f l =
    go 0 f l
    where
        go :: Int -> ((Int, a) -> b) -> [a] -> [b]
        go _ _ [] = []
        go i fun (x:xs) =
            fun (i,x) : go (i + 1) fun xs

main :: IO ()
main = do
    raw <- getContents
    print . foldAll . parse $ raw
    print . length . foldAll . parse $ raw
    putStrLn . printDots . foldAll . parse $ raw