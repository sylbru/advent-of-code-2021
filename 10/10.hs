import Data.List (sort)
import Data.Maybe (fromJust, mapMaybe)

data Diagnostic
    = Corrupted Char -- Syntax error on this char
    | Incomplete [Char] -- Incomplete line with this stack of open chunks
    | Correct
    deriving (Show)

matchingChars :: [(Char, Char)]
matchingChars =
    [ ('(', ')')
    , ('[', ']')
    , ('{', '}')
    , ('<', '>')
    ]


syntaxErrorScore :: String -> Int
syntaxErrorScore input =
    toErrorScore . map diagnoseLine . lines $ input

toErrorScore :: [Diagnostic] -> Int
toErrorScore diagnostics =
    sum . map value $ diagnostics
    where
        value (Corrupted char) =
            case char of
                ')' -> 3
                ']' -> 57
                '}' -> 1197
                '>' -> 25137
                _ -> 0
        value _ = 0


completionScore :: String -> Int
completionScore input =
    toCompletionScore . map diagnoseLine . lines $ input

toCompletionScore :: [Diagnostic] -> Int
toCompletionScore diagnostics =
    let scores = filter (/= 0) $ map value diagnostics
    in
    sort scores !! (length scores `div` 2)
    where
        value (Incomplete []) = 0
        value (Incomplete stack) = completionScoreLine . autocomplete $ stack
        value _ = 0

completionScoreLine :: [Char] -> Int
completionScoreLine =
    foldl fn 0
    where
        fn acc char =
            acc * 5
                + (case char of
                    ')' -> 1
                    ']' -> 2
                    '}' -> 3
                    '>' -> 4
                    _ -> 0
                )

autocomplete :: [Char] -> [Char]
autocomplete openChunks =
    mapMaybe matchingChar openChunks
    where
        matchingChar openingChar =
            lookup openingChar matchingChars

diagnoseLine :: String -> Diagnostic
diagnoseLine [] = Correct
diagnoseLine (stackInit:line) =
    go [stackInit] line
    where
        go :: [Char] -> String -> Diagnostic
        go [] [] = Correct
        go stack [] = Incomplete stack
        go [] (c:_) = Corrupted c
        go stack@(s:stackRest) (c:lineRest) =
            if isOpeningChar c then
                go (c:stack) lineRest
            else if isClosingChar c then
                if c `matchesOpeningChar` s then
                    go stackRest lineRest
                else
                    Corrupted c
            else
                go stack lineRest

isOpeningChar :: Char -> Bool
isOpeningChar c = elem c (map fst matchingChars)

isClosingChar :: Char -> Bool
isClosingChar c = elem c (map snd matchingChars)

matchesOpeningChar :: Char -> Char -> Bool
matchesOpeningChar c s = elem (s, c) matchingChars


main :: IO ()
main = do
    raw <- getContents
    print . syntaxErrorScore $ raw
    print . completionScore $ raw