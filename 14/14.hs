import Data.Map.Strict (Map, empty, alter, toList)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe, fromJust, maybe)

type Element = Char
type Polymer = [Element]
type Rule = ((Element, Element), Element)

parseInput :: String -> (Polymer, [Rule])
parseInput input =
    case splitOn "\n\n" input of
        template:rules:[] -> (template, parseRules rules)
        _ -> ([], [])

parseRules :: String -> [Rule]
parseRules input =
    mapMaybe parseRule . lines $ input

parseRule :: String -> Maybe Rule
parseRule input =
    case splitOn " -> " input of
        (pair@[p1,p2]) : [replacement] : [] ->
            Just ((p1,p2), replacement)

        _ -> Nothing

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

    print $ count finalPolymer