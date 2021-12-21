type Score = Int
type InitialState = (Position, Position)
type Position = Int
type DiceState = Int

example :: InitialState
example = (4, 8)

input :: InitialState
input = (10, 1)


deterministicRoll :: (Position, DiceState) -> (Position, DiceState)
deterministicRoll (pos, diceAt) =
    let
        diceValue = sum [diceAt..diceAt+2] -- or (diceAt+2)*(diceAt+3) / 2 - diceAt*(diceAt-1) / 2
                                           -- but who cares?
        newPos = 1 + ((pos + diceValue - 1)  `mod` 10)
        newDiceAt = diceAt + 3
    in
    (newPos, newDiceAt)

initialDice :: DiceState
initialDice = 1

play :: InitialState -> (Score, Score, Int)
play initialState =
    go initialState (0,0) initialDice True 0
    where
        go (pos1, pos2) (score1, score2) diceAt firstPlayersTurn rolls =
            if score1 >= 1000 || score2 >= 1000 then
                (score1, score2, rolls)
            else
                let
                    pos = if firstPlayersTurn then pos1 else pos2
                    (newPos, newDiceAt) = deterministicRoll (pos, diceAt)
                    newPositions = if firstPlayersTurn then (newPos, pos2) else (pos1, newPos)
                    newScores = if firstPlayersTurn then (score1 + newPos, score2) else (score1, score2 + newPos)
                in
                go newPositions newScores newDiceAt (not firstPlayersTurn) (rolls + 3)

answer :: (Score, Score, Int) -> Int
answer (score1, score2, rolls) =
    rolls * (min score1 score2)

main :: IO ()
main = do
    print . answer . play $ input
