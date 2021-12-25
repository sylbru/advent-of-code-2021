import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe

data SeaCucumber = EastFacing | SouthFacing
type SeaFloor = [[Maybe SeaCucumber]]

parseInput :: String -> SeaFloor
parseInput input =
    map (map parseCell) . lines $ input

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
    moveSouthFacingHerd . moveEastFacingHerd $ floor

end :: [a] -> a
end = head . reverse

moveEastFacingHerd :: SeaFloor -> SeaFloor
moveEastFacingHerd floor =
    -- map ((drop 1) . (\row@(f:_) -> moveRow [] f (end row : row))) floor
    map (\row@(f:_) -> moveRow [] f row) floor
    where
        moveRow :: [Maybe SeaCucumber] -> Maybe SeaCucumber -> [Maybe SeaCucumber] -> [Maybe SeaCucumber]
        moveRow acc first (Just EastFacing:Nothing:rest) =
            moveRow (Just EastFacing:Nothing:acc) first rest
        moveRow acc first (current:next:rest) =
            moveRow (current:acc) first (next:rest)
        moveRow acc first (last:[]) =
            reverse (last:acc)
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


moveSouthFacingHerd :: SeaFloor -> SeaFloor
moveSouthFacingHerd = id

main :: IO ()
main = do
    raw <- getContents
    putStrLn . seaFloorToString . move . parseInput $ "...>>>..."
