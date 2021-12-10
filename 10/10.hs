import Data.Maybe (mapMaybe)

syntaxErrorScore :: String -> Int
syntaxErrorScore input =
    toScore . mapMaybe findSyntaxError . lines $ input

findSyntaxError :: String -> Maybe Char
findSyntaxError [] = Nothing
findSyntaxError (stackInit:line) =
    go [stackInit] line
    where
        go :: [Char] -> String -> Maybe Char
        go _ [] = Nothing
        go [] (c:_) = Just c
        go stack@(s:stackRest) (c:lineRest) =
            if isOpeningChar c then
                go (c:stack) lineRest
            else if isClosingChar c then
                if c `matchesOpeningChar` s then
                    go stackRest lineRest
                else
                    Just c
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

toScore :: [Char] -> Int
toScore errors =
    sum $ map value errors
    where
        value :: Char -> Int
        value char =
            case char of
                ')' -> 3
                ']' -> 57
                '}' -> 1197
                '>' -> 25137
                _ -> 0

main :: IO ()
main = do
    raw <- getContents
    print . syntaxErrorScore $ raw