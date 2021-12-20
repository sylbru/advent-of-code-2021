import Data.Map.Strict (Map, fromList, foldrWithKey, lookup)
-- import Data.List (map)

data Pixel = Light | Dark deriving (Eq)
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
        minX = foldrWithKey (\(x,_) _ acc -> min acc x) 0 image
        minY = foldrWithKey (\(_,y) _ acc -> min acc y) 0 image
        maxX = foldrWithKey (\(x,_) _ acc -> max acc x) 0 image
        maxY = foldrWithKey (\(_,y) _ acc -> max acc y) 0 image
        (w, h) = (1 + maxX - minX, 1 + maxY - minY)
        emptyImage = (replicate h (replicate w Dark))

        imageAsList =
            indexedMap
                (\(y, row) ->
                    indexedMap
                        (\(x, _) ->
                            case Data.Map.Strict.lookup (x,y) image of
                                Just pixel -> pixel
                                Nothing -> Dark
                        )
                        row
                )
                emptyImage
    in
    unlines . map (map printPixel) $ imageAsList

printPixel :: Pixel -> Char
printPixel pixel =
    case pixel of
        Light -> '█'
        Dark -> ' '

main :: IO ()
main = do
    raw <- getContents
    let (algorithm, inputImage) = parseInput raw
    putStrLn $ printImage inputImage
