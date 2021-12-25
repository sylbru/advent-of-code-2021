import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe

data SeaCucumber = EastFacing | SouthFacing
type SeaFloor = ((Int, Int), Map (Int, Int) SeaCucumber)

parseInput :: String -> SeaFloor
parseInput input =
    let rows = lines input
        dimensions = (length $ head rows, length rows)
        grid =
            Map.fromList . Maybe.mapMaybe (\(key, maybeValue) ->
                case maybeValue of
                    Just value -> Just (key, value)
                    Nothing -> Nothing
                ) . concat $
            indexedMap
                (\(y, row) ->
                    indexedMap
                        (\(x, cell) ->
                            ((x,y), parseCell cell)
                        )
                        row
                )
                rows
    in
    (dimensions, grid)

indexedMap :: ((Int, a) -> b) -> [a] -> [b]
indexedMap f l =
    go 0 f l
    where
        go :: Int -> ((Int, a) -> b) -> [a] -> [b]
        go _ _ [] = []
        go i fun (x:xs) =
            fun (i,x) : go (i + 1) fun xs


parseCell :: Char -> Maybe SeaCucumber
parseCell char =
    case char of
        '>' -> Just EastFacing
        'v' -> Just SouthFacing
        _ -> Nothing

seaFloorToString :: SeaFloor -> String
seaFloorToString ((width, height), floor) =
    let
        floorAsList =
            map
                (\y ->
                    map (\x -> Map.lookup (x,y) floor) [0..width - 1]
                )
                [0..height - 1]
    in
    unlines . map (map cellToString) $ floorAsList

cellToString :: Maybe SeaCucumber -> Char
cellToString cell =
    case cell of
        Nothing -> '.'
        Just EastFacing -> '>'
        Just SouthFacing -> 'v'

step :: SeaFloor -> SeaFloor
step floor =
    moveSouthFacingHerd . moveEastFacingHerd $ floor

moveEastFacingHerd :: SeaFloor -> SeaFloor
moveEastFacingHerd = id

moveSouthFacingHerd :: SeaFloor -> SeaFloor
moveSouthFacingHerd = id

main :: IO ()
main = do
    raw <- getContents
    putStrLn . seaFloorToString . step . parseInput $ "...>>>>>..."
