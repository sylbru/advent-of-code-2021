import Data.Tuple (fst)
import qualified Data.List.Split as S
import Data.Char (isSpace)
import Data.List (transpose)

type Board = [[Cell]]
type Cell = (Int, Bool)

testInput :: String
testInput = "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n\n22 13 17 11  0\n 8  2 23  4 24\n21  9 14 16  7\n 6 10  3 18  5\n 1 12 20 15 19\n\n 3 15  0  2 22\n 9 18 13 17  5\n19  8  7 25 23\n20 11 10 24  4\n14 21 16 12  6\n\n14 21 17 24  4\n10 16 15  9 19\n18  8 23 26 20\n22 11 13  6  5\n 2  0 12  3  7\n"

parseInput :: String -> ([Int], [Board])
parseInput input =
    (parseRandomNumbers randomNumbers, map parseBoard boards)
    where
        randomNumbers : boards = S.split (dropCondenseOnSublist "\n\n") input

parseRandomNumbers :: String -> [Int]
parseRandomNumbers =
    map read . S.splitOn ","

parseBoard :: String -> Board
parseBoard =
    map parseBoardRow . S.splitOn "\n" . trim

parseBoardRow :: String -> [Cell]
parseBoardRow =
    map parseBoardCell . S.split (dropCondenseOneOf " ") . trim

parseBoardCell :: String -> Cell
parseBoardCell =
    (\i -> (i, False)) . read


drawNumber :: [Int] -> [Board] -> ([Int], [Board])
drawNumber numbers boards =
    ([],[])

play :: ([Int], [Board]) -> (Int, Board)
play ((number:rest), boards) =
    let
        newBoards = map (markNumber number) boards
    in
    case filter winning newBoards of
        [] -> play (rest, newBoards)
        [winningBoard] -> (number, winningBoard)
        -- severalWinningBoards -> ?

markNumber :: Int -> Board -> Board
markNumber number board =
    map
        (\row ->
            map
                (\(n, marked) ->
                    if n == number then (n, True) else (n, marked))
                row
        )
        board

winning :: Board -> Bool
winning board =
    any isComplete board || any isComplete (transpose board)

score :: (Int, Board) -> Int
score (finalNumber, board) =
    finalNumber * sumUnmarkedNumbers
    where
        sumUnmarkedNumbers =
            foldl (+) 0 unmarkedNumbers

        unmarkedNumbers =
            map fst . filter isUnmarked . concat $ board

isComplete :: [Cell] -> Bool
isComplete row =
    all isMarked row

isMarked :: Cell -> Bool
isMarked (_, marked) =
    marked == True

isUnmarked :: Cell -> Bool
isUnmarked (_, marked) =
    marked == False

trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace

dropCondenseOneOf :: [Char] -> S.Splitter Char
dropCondenseOneOf delim =
    S.dropDelims . S.condense $ S.oneOf delim

dropCondenseOnSublist :: [Char] -> S.Splitter Char
dropCondenseOnSublist delim =
    S.dropDelims . S.condense $ S.onSublist delim


main :: IO ()
main = do
    raw <- getContents
    print . score . play . parseInput $ raw