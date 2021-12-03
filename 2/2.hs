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
    [ Forward 5 
    , Down 5
    , Forward 8
    , Up 3
    , Down 8
    , Forward 2
    ]

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
    -- raw <- getContents
    print . result . position $ testInput