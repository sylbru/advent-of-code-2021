import Data.Maybe (mapMaybe)

type Score = Int
type InitialState = (Position, Position)
type Position = Int
type DiceState = Int
data Game = Game { positions :: (Position, Position), scores :: (Score, Score) } deriving (Show)

example :: InitialState
example = (4, 8)

input :: InitialState
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

roll3 :: (Position, DiceState) -> (Position, DiceState)
roll3 (pos, diceAt) =
    let
        diceValue = sum [diceAt..diceAt+2] -- or (diceAt+2)*(diceAt+3) / 2 - diceAt*(diceAt-1) / 2
                                           -- but who cares?
        newPos = 1 + ((pos + diceValue - 1)  `mod` 10)
        newDiceAt = 1 + (diceAt + 3 - 1 `mod` 3)
    in
    (newPos, newDiceAt)

initialDice :: DiceState
initialDice = 1

play1 :: InitialState -> (Score, Score, Int)
play1 initialState =
    go initialState (0,0) initialDice True 0
    where
        go (pos1, pos2) (score1, score2) diceAt firstPlayersTurn rolls =
            if score1 >= 1000 || score2 >= 1000 then
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

play2 :: InitialState -> (Int, Int)
play2 initialState =
    go ((0,0), [Game { positions = initialState, scores = (0,0) }]) True
    where
        go :: ((Int, Int), [Game]) -> Bool -> (Int, Int)
        go (winningGames, ongoingGames) firstPlayersTurn =
            if null ongoingGames then
                winningGames
            else
                let
                    newOngoingGames :: [Game]
                    newOngoingGames = concat $ map
                        (\game ->
                            map (\rollSum ->
                                let
                                    (pos1, pos2) = positions game
                                    (score1, score2) = scores game
                                    move by from = 1 + ((from + by - 1) `mod` 10)

                                    newPositions = if firstPlayersTurn then
                                        (move rollSum pos1, pos2) else (pos1, move rollSum pos2)
                                    newScores = if firstPlayersTurn then
                                        (score1 + (fst newPositions), score2) else (score1, score2 + (snd newPositions))
                                in
                                Game { positions = newPositions, scores = newScores}
                            )
                            multiRollsSums
                        )
                        ongoingGames
                in
                go (closeGames (winningGames, newOngoingGames)) firstPlayersTurn


closeGames :: ((Int, Int), [Game]) -> ((Int, Int), [Game])
closeGames (wins, ongoing) =
    let
        won = mapMaybe (\game ->
            if fst (scores game) >= 21 then
                Just True
            else if snd (scores game) >= 21 then
                Just False
            else
                Nothing
            )
            ongoing
        newWins = (fst wins + (length $ filter id won), snd wins + (length $ filter (not . id) won))
        newOngoing = filter
            (\game -> fst (scores game) < 21 && snd (scores game) < 21)
            ongoing
    in
    (newWins, newOngoing)

-- gameOver :: Game -> Bool
-- gameOver (Game { positions }) =
--     fst positions >= 21 || snd positions >= 21

multiRollsSums :: [Int]
multiRollsSums =
    map sum
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



answer2 :: (Int, Int) -> Int
answer2 winningGames =
    max (fst winningGames) (snd winningGames)

main :: IO ()
main = do
    -- print . answer1 . play1 $ example
    print . answer2 . play2 $ example
