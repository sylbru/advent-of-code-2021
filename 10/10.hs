data Diagnostic
    = Corrupted Char -- Syntax error on this char
    | Incomplete [Char] -- Incomplete line with this stack of open chunks
    | Correct

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
isOpeningChar c = elem c ['(', '[', '{', '<']

isClosingChar :: Char -> Bool
isClosingChar c = elem c [')', ']', '}', '>']

matchesOpeningChar :: Char -> Char -> Bool
matchesOpeningChar c s =
    case (s, c) of
        ('(', ')') -> True
        ('[', ']') -> True
        ('{', '}') -> True
        ('<', '>') -> True
        _ -> False

main :: IO ()
main = do
    raw <- getContents
    print . syntaxErrorScore $ raw