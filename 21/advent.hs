{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import Data.List
import Data.Maybe (mapMaybe)
import Control.DeepSeq (force, NFData)
import GHC.Generics (Generic)

type Score = Int
type InitialPositions = (Position, Position)
type Position = Int
type DiceState = Int
data Game = Game
    { positions :: (Position, Position)
    , scores :: (Score, Score)
    , occurrences :: Int
    }
    deriving (Eq, Show, Generic, NFData)

example :: InitialPositions
example = (4, 8)

input :: InitialPositions
input = (10, 1)


roll100 :: (Position, DiceState) -> (Position, DiceState)
roll100 (pos, diceAt) =
    let
        diceValue = sum [diceAt..diceAt+2] -- or (diceAt+2)*(diceAt+3) / 2 - diceAt*(diceAt-1) / 2
                                           -- but who cares?
        newPos = 1 + ((pos + diceValue - 1)  `mod` 10)
        newDiceAt = 1 + (diceAt + 3 - 1 `mod` 100)
    in
    (newPos, newDiceAt)

initialDice :: DiceState
initialDice = 1

finalScore1 :: Int
finalScore1 = 1000

play1 :: InitialPositions -> (Score, Score, Int)
play1 initialPositions =
    go initialPositions (0,0) initialDice True 0
    where
        go (pos1, pos2) (score1, score2) diceAt firstPlayersTurn rolls =
            if score1 >= finalScore1 || score2 >= finalScore1 then
                (score1, score2, rolls)
            else
                let
                    pos = if firstPlayersTurn then pos1 else pos2
                    (newPos, newDiceAt) = roll100 (pos, diceAt)
                    newPositions = if firstPlayersTurn then (newPos, pos2) else (pos1, newPos)
                    newScores = if firstPlayersTurn then (score1 + newPos, score2) else (score1, score2 + newPos)
                in
                go newPositions newScores newDiceAt (not firstPlayersTurn) (rolls + 3)

answer1 :: (Score, Score, Int) -> Int
answer1 (score1, score2, rolls) =
    rolls * (min score1 score2)

finalScore2 :: Int
finalScore2 = 8

play2 :: InitialPositions -> (Int, Int)
play2 initialPositions =
    go ((0,0), [Game { positions = initialPositions, scores = (0,0), occurrences = 1 }]) True
    where
        go :: ((Int, Int), [Game]) -> Bool -> (Int, Int)
        go (winningGames, ongoingGames) firstPlayersTurn =
            if null ongoingGames then
                winningGames
            else
                let
                    newOngoingGames :: [Game]
                    newOngoingGames = force $ concat $ map
                        (\game ->
                            map (\(rollSum, occ) ->
                                let
                                    (pos1, pos2) = positions game
                                    (score1, score2) = scores game
                                    move by from = 1 + ((from + by - 1) `mod` 10)

                                    newPositions = if firstPlayersTurn then
                                        (move rollSum pos1, pos2) else (pos1, move rollSum pos2)
                                    newScores = if firstPlayersTurn then
                                        (score1 + (fst newPositions), score2) else (score1, score2 + (snd newPositions))
                                in
                                Game { positions = newPositions, scores = newScores, occurrences = occurrences game * occ }
                            )
                            multiRollsSums
                        )
                        ongoingGames
                in
                go (force $ closeGames (winningGames, newOngoingGames)) (not firstPlayersTurn)


closeGames :: ((Int, Int), [Game]) -> ((Int, Int), [Game])
closeGames (wins, ongoing) =
    let
        won = mapMaybe (\game ->
            if fst (scores game) >= finalScore2 then
                Just (True, occurrences game)
            else if snd (scores game) >= finalScore2 then
                Just (False, occurrences game)
            else
                Nothing
            )
            ongoing
        newWins =
            ( fst wins + (sum $ map snd $ filter (id . fst) won)
            , snd wins + (sum $ map snd $ filter (not . id . fst) won)
            )
        newOngoing = filter
            (\game -> fst (scores game) < finalScore2 && snd (scores game) < finalScore2)
            ongoing
    in
    (newWins, newOngoing)

multiRollsSums :: [(Int, Int)]
multiRollsSums =
    force $
        map (\g -> (head g, length g)) . group . sort . map sum $
            [ [1,1,1], [1,1,2], [1,1,3]
            , [1,2,1], [1,2,2], [1,2,3]
            , [1,3,1], [1,3,2], [1,3,3]
            , [2,1,1], [2,1,2], [2,1,3]
            , [2,2,1], [2,2,2], [2,2,3]
            , [2,3,1], [2,3,2], [2,3,3]
            , [3,1,1], [3,1,2], [3,1,3]
            , [3,2,1], [3,2,2], [3,2,3]
            , [3,3,1], [3,3,2], [3,3,3]
            ]
-- multiRollsSums :: [Int]
-- multiRollsSums =
--     force $ map sum
--         [ [1,1]
--         , [1,2]
--         , [2,1]
--         , [2,2]
--         ]
-- multiRollsSums :: [Int]
-- multiRollsSums =
--     [1..3]

answer2 :: (Int, Int) -> Int
answer2 winningGames =
    max (fst winningGames) (snd winningGames)

main :: IO ()
main = do
    -- print . answer1 . play1 $ example
    print . play2 $ example
