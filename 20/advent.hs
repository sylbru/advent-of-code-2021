import Data.Map.Strict (Map, fromList, foldrWithKey, lookup, filter)
import Data.Maybe (fromMaybe)

data Pixel = Light | Dark deriving (Eq, Show)
type Image = Map (Int, Int) Pixel
type Algorithm = [Pixel]

parseInput :: String -> (Algorithm, Image)
parseInput input =
    case lines input of
        algorithm:"":image ->
            (parseAlgorithm algorithm, parseImage image)

parseImage :: [String] -> Image
parseImage input =
    fromList . concat $
    indexedMap
        (\(y, row) ->
            indexedMap
                (\(x, cell) ->
                    ((x,y), parsePixel cell)
                )
                row
        )
        input

indexedMap :: ((Int, a) -> b) -> [a] -> [b]
indexedMap f l =
    go 0 f l
    where
        go :: Int -> ((Int, a) -> b) -> [a] -> [b]
        go _ _ [] = []
        go i fun (x:xs) =
            fun (i,x) : go (i + 1) fun xs


parsePixel :: Char -> Pixel
parsePixel char =
    if char == '#' then
        Light
    else
        Dark

parseAlgorithm :: String -> Algorithm
parseAlgorithm input =
    map parsePixel input



printImage :: Image -> String
printImage image =
    let
        (minX, maxX) = foldrWithKey (\(x,_) _ acc -> (min (fst acc) x, max (snd acc) x)) (0,0) image
        (minY, maxY) = foldrWithKey (\(_,y) _ acc -> (min (fst acc) y, max (snd acc) y)) (0,0) image
        imageAsList =
            map
                (\y ->
                    map
                        (\x ->
                            case Data.Map.Strict.lookup (x,y) image of
                                Just pixel -> pixel
                                Nothing -> Dark
                        )
                        [minX..maxX]
                )
                [minY..maxY]
    in
    unlines . map (map printPixel) $ imageAsList

printPixel :: Pixel -> Char
printPixel pixel =
    case pixel of
        Light -> '█'
        Dark -> ' '

enhance :: Algorithm -> Pixel -> Image -> Image
enhance algorithm background inputImage =
    let
        (minX, maxX) = foldrWithKey (\(x,_) _ acc -> (min (fst acc) x, max (snd acc) x)) (0,0) inputImage
        (minY, maxY) = foldrWithKey (\(_,y) _ acc -> (min (fst acc) y, max (snd acc) y)) (0,0) inputImage
    in
    fromList . concat $ map
        (\y ->
            map
                (\x ->
                    ((x,y), enhancePixel (x,y) algorithm background inputImage)
                )
                [minX - 1 .. maxX + 1]
        )
        [minY - 1 .. maxY + 1]

enhancePixel :: (Int, Int) -> Algorithm -> Pixel -> Image -> Pixel
enhancePixel (x,y) algorithm background image =
    let
        inputPixels =
            map
                (\coords -> fromMaybe background $ Data.Map.Strict.lookup coords image)
                [ (x - 1, y - 1) , (x, y - 1) , (x + 1, y - 1)
                , (x - 1, y) ,     (x, y) ,     (x + 1, y)
                , (x - 1, y + 1) , (x, y + 1) , (x + 1, y + 1)
                ]
    in
    algorithm !! (pixelsToInt inputPixels)

pixelsToInt :: [Pixel] -> Int
pixelsToInt pixels =
    go 0 (reverse pixels)
    where
        go :: Int -> [Pixel] -> Int
        go _ [] = 0
        go col (first:rest) =
            case first of
                Light ->
                    2 ^ col + (go (col + 1) rest)
                Dark ->
                    go (col + 1) rest

enhanceNTimes :: Int -> Pixel -> Algorithm -> Image -> Image
enhanceNTimes n background algorithm inputImage =
    if n <= 0 then
        inputImage
    else
        enhanceNTimes
            (n - 1)
            (algorithm !! (pixelsToInt $ replicate 9 background))
            algorithm
            (enhance algorithm background inputImage)

countLitPixels :: Image -> Int
countLitPixels image =
    length $ Data.Map.Strict.filter ((==) Light) image

main :: IO ()
main = do
    raw <- getContents
    let (algorithm, inputImage) = parseInput raw
    -- putStrLn $ printImage inputImage
    let enhanced = enhanceNTimes 2 Dark algorithm inputImage
    -- putStrLn $ printImage enhanced
    print $ countLitPixels enhanced
