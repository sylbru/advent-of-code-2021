import qualified Data.List.Split as S
import Data.List (transpose)

type Point = (Int, Int)

testInput :: String
testInput =
    "0,9 -> 5,9\n8,0 -> 0,8\n9,4 -> 3,4\n2,2 -> 2,1\n7,0 -> 7,4\n6,4 -> 2,0\n0,9 -> 2,9\n3,4 -> 1,4\n0,0 -> 8,8\n5,5 -> 8,2\n"

parseInput :: String -> [(Point, Point)]
parseInput =
    map parseLine . lines
    where
        parseLine :: String -> (Point, Point)
        parseLine =
            (\[start, end] -> (parsePoint start, parsePoint end)) . S.splitOn " -> "

        parsePoint :: String -> Point
        parsePoint =
            listTo2Tuple . map read . S.splitOn "," 


listTo2Tuple :: [a] -> (a, a)
listTo2Tuple (a1:a2:_) = (a1, a2)

dangerMap :: [(Point, Point)] -> [[Int]]
dangerMap input =
    foldl addLine initialMap input
    where
        initialMap :: [[Int]]
        initialMap = 
            emptyMap (widthFrom input) (heightFrom input)

addLine :: [[Int]] -> (Point, Point) -> [[Int]]
addLine acc ((x1,y1), (x2,y2)) =
    if y1 == y2 then
        addHorizontalLine y1 (x1,x2) acc
    else if x1 == x2 then
        addVerticalLine x1 (y1,y2) acc
    else
        -- Diagonal line, ignore for now
        acc

addHorizontalLine :: Int -> (Int, Int) -> [[Int]] -> [[Int]]
addHorizontalLine x (start, end) acc =
    indexedMap
        (\(i,l) ->
            if i == x then
                indexedMap
                    (\(j,c) ->
                        -- we assume start and end might be swapped
                        if j >= start && j <= end || j >= end && j <= start then
                            c + 1
                        else
                            c
                    )
                    l
            else
                l
        )
        acc


addVerticalLine :: Int -> (Int, Int) -> [[Int]] -> [[Int]]
addVerticalLine y (start, end) =
    transpose . (addHorizontalLine y (start, end)) . transpose

indexedMap :: ((Int, a) -> b) -> [a] -> [b]
indexedMap f l =
    go 0 f l
    where
        go :: Int -> ((Int, a) -> b) -> [a] -> [b]
        go _ _ [] = []
        go i fun (x:xs) =
            fun (i,x) : go (i + 1) fun xs

emptyMap :: Int -> Int -> [[Int]]
emptyMap width height =
    replicate height . replicate width $ 0

widthFrom :: [(Point, Point)] -> Int
widthFrom =
    (1 +) . maximum . concat . map (\((x1,_) , (x2,_)) -> [x1, x2])

heightFrom :: [(Point, Point)] -> Int
heightFrom =
    (1 +) . maximum . concat . map (\((_,y1) , (_,y2)) -> [y1, y2])


countDangerPoints :: [(Point, Point)] -> Int
countDangerPoints =
    length . filter (> 1) . concat . dangerMap


main :: IO ()
main = do
    raw <- getContents
    print . countDangerPoints . parseInput $ raw