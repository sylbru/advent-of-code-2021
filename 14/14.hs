import Data.Map.Strict (Map, empty, alter, toList)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe, fromJust, maybe)

data Element = N | C | B | H deriving (Eq, Show, Ord)

type Polymer = [Element]
type Rule = ((Element, Element), Element)

parseInput :: String -> (Polymer, [Rule])
parseInput input =
    case splitOn "\n\n" input of
        template:rules:[] -> (parseTemplate template, parseRules rules)
        _ -> ([], [])

parseTemplate :: String -> Polymer
parseTemplate =
    mapMaybe parseElement

parseElement :: Char -> Maybe Element
parseElement char =
    case char of
        'B' -> Just B
        'C' -> Just C
        'H' -> Just H
        'N' -> Just N
        _ -> Nothing

parseRules :: String -> [Rule]
parseRules input =
    mapMaybe parseRule $ splitOn "\n" input

parseRule :: String -> Maybe Rule
parseRule input =
    case splitOn " -> " input of
        (pair@[_,_]) : ([replacement]) : [] ->
            Just ((listTo2Tuple $ mapMaybe parseElement pair), fromJust $ parseElement replacement)

        _ -> Nothing

listTo2Tuple :: [a] -> (a, a)
listTo2Tuple (a1:a2:_) = (a1, a2)

step :: [Rule] -> Polymer -> Polymer
step rules_ polymer_ =
    case polymer_ of
        x:x':xs ->
            let
                firstPair =
                    case lookup (x,x') rules_ of
                        Just el -> [x,el]

                        Nothing ->
                            [x,x']
            in
            firstPair ++ (step rules_(x':xs))


        _ ->
            polymer_

printPolymer :: Polymer -> String
printPolymer = concat . map show

steps :: Int -> [Rule] -> Polymer -> Polymer
steps 0 _ polymer = polymer
steps n rules polymer =
    steps (n - 1) rules (step rules polymer)

countQuantities :: Polymer -> [(Element, Int)]
countQuantities polymer =
    toList $ foldr countElement empty polymer
    where
        countElement :: Element -> Map Element Int -> Map Element Int
        countElement cur acc =
            alter
                (\c -> Just (1 + withDefault 0 c))
                cur
                acc

withDefault :: a -> Maybe a -> a
withDefault default_ maybe_ =
    case maybe_ of
        Just x -> x
        Nothing -> default_

quantityMostCommon :: [(Element, Int)] -> Int
quantityMostCommon = maximum . map snd

quantityLeastCommon :: [(Element, Int)] -> Int
quantityLeastCommon = minimum . map snd

count :: Polymer -> Int
count polymer =
    quantityMostCommon quantities - quantityLeastCommon quantities
    where
        quantities = seq () (countQuantities polymer)

main :: IO ()
main = do
    raw <- getContents
    let (polymer, rules) = parseInput raw
    let finalPolymer = steps 10 rules polymer

    putStrLn . printPolymer $ finalPolymer
    print $ count finalPolymer