data Pixel = Light | Dark deriving (Eq)
type Image = [[Pixel]]
type Algorithm = [Pixel]

parseInput :: String -> (Algorithm, Image)
parseInput input =
    case lines input of
        algorithm:"":image ->
            (parseAlgorithm algorithm, parseImage image)

parsePixel :: Char -> Pixel
parsePixel char =
    if char == '#' then
        Light
    else
        Dark

parseAlgorithm :: String -> Algorithm
parseAlgorithm input =
    map parsePixel input

parseImage :: [String] -> Image
parseImage input =
    map (map parsePixel) input

printImage :: Image -> String
printImage image =
    unlines . map (map printPixel) $ image
    where
        printPixel pixel =
            case pixel of
                Light -> '█'
                Dark -> ' '

main :: IO ()
main = do
    raw <- getContents
    let (algorithm, inputImage) = parseInput raw
    putStrLn $ printImage inputImage
