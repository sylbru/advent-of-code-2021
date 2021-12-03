data Position = Position
    { horizontal :: Int
    , depth :: Int
    }

data Command
    = Forward Int
    | Down Int 
    | Up Int

testInput :: [Command]
testInput =
    parseInput "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2"

parseInput :: String -> [Command]
parseInput input =
    map parseLine (lines input)
    where
        parseLine :: String -> Command
        parseLine line =
            case words line of
                ["forward", val] -> Forward (read val)
                ["down", val] -> Down (read val)
                ["up", val] -> Up (read val)

initial :: Position
initial =
    Position { horizontal = 0, depth = 0 }

position :: [Command] -> Position
position commands =
    foldl interpret initial commands
    where
        interpret :: Position -> Command -> Position
        interpret pos command =
            case command of
                Forward l -> pos { horizontal = horizontal pos + l }
                Down l -> pos { depth = depth pos + l }
                Up l -> pos { depth = depth pos - l }

result :: Position -> Int
result position =
    horizontal position * depth position

main :: IO ()
main = do
    raw <- getContents
    print . result . position . parseInput $ raw