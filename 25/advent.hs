import qualified Data.List as List
import qualified Data.Maybe as Maybe

data SeaCucumber = EastFacing | SouthFacing deriving (Eq, Show)
type SeaFloor = [[Maybe SeaCucumber]]

parseInput :: String -> SeaFloor
parseInput input =
    map (map parseCell) . lines $ input

-- instance Show SeaCucumber

parseCell :: Char -> Maybe SeaCucumber
parseCell char =
    case char of
        '>' -> Just EastFacing
        'v' -> Just SouthFacing
        _ -> Nothing

seaFloorToString :: SeaFloor -> String
seaFloorToString floor =
    unlines . map (map cellToString) $ floor

cellToString :: Maybe SeaCucumber -> Char
cellToString cell =
    case cell of
        Nothing -> '.'
        Just EastFacing -> '>'
        Just SouthFacing -> 'v'

move :: SeaFloor -> SeaFloor
move floor =
    moveSouthFacingHerd . moveHerd EastFacing $ floor

end :: [a] -> a
end = head . reverse

moveHerd :: SeaCucumber -> SeaFloor -> SeaFloor
moveHerd cucumberType floor =
    -- map ((drop 1) . (\row@(f:_) -> moveRow [] f (end row : row))) floor
    map (\row@(f:_) -> moveRow [] f row) floor
    where
        moveRow :: [Maybe SeaCucumber] -> Maybe SeaCucumber -> [Maybe SeaCucumber] -> [Maybe SeaCucumber]
        moveRow acc first (Just c:Nothing:rest) | c == cucumberType =
            moveRow (Just c:Nothing:acc) first rest
        moveRow acc first (current:next:rest) =
            moveRow (current:acc) first (next:rest)
        moveRow acc Nothing (Just c:[]) | c == cucumberType =
            setHead (Just c) . reverse $ (Nothing:acc)
        moveRow acc _ (lst:[]) =
            reverse (lst:acc)
        moveRow acc _ [] =
            reverse acc

            -- >.
            -- >.>.v.>>.
        --     case
        -- moveRow acc first (current:after:rest) =
        --     case (before, current) of
        --         (Just EastFacing, Nothing) ->
        --             moveRow (Just EastFacing:acc) first
        --         (_, Nothing)
        --         (_, Nothing)
        -- moveRow acc first (before:Just EastFacing:after:rest) =
        -- moveRow acc first (before:Just EastFacing:after:rest) =
        --     if Maybe.isNothing after then
        --         moveRow (acc ++ ) first

        --     moveRow first (after:rest)
        -- moveRow acc first floor_@(_:after:rest) = floor_


setHead :: a -> [a] -> [a]
setHead newHead (_:tail) = (newHead:tail)

moveSouthFacingHerd :: SeaFloor -> SeaFloor
moveSouthFacingHerd = List.transpose . moveHerd SouthFacing . List.transpose

moveNSteps :: Int -> SeaFloor -> SeaFloor
moveNSteps n floor =
    (iterate move floor) !! n

moveUntilSettled :: SeaFloor -> (SeaFloor, Int)
moveUntilSettled floor =
    go 0 floor
    where
        go :: Int -> SeaFloor -> (SeaFloor, Int)
        go i f =
            let step = move f
            in
            if step == f then
                (step, i + 1)
            else
                go (i + 1) step

main :: IO ()
main = do
    raw <- getContents
    -- putStrLn . seaFloorToString . move . parseInput $ ".>"
    let (endFloor, steps) = moveUntilSettled . parseInput $ raw
    print steps
    putStrLn $ seaFloorToString endFloor
    -- putStrLn . seaFloorToString . List.transpose . parseInput $ "...>>>...>\nv........."
    -- putStrLn . seaFloorToString . moveHerd SouthFacing . List.transpose . parseInput $ "...>>>...>\nv........."
    -- putStrLn . seaFloorToString . List.transpose . moveHerd SouthFacing . List.transpose . parseInput $ "...>>>...>\nv........."
    -- putStrLn . seaFloorToString . move . parseInput $ "...>>>...>\nv........."
