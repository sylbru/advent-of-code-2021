import Data.Map.Strict (Map, fromList, map, foldrWithKey, lookup)
import Data.Set (Set)
import Data.Maybe (fromJust)
import Data.List (permutations, nub)

type Cavern = Map (Int, Int) Int
data Direction = D | R deriving (Eq, Show, Ord)

-- Supposons que les chemins sont toujours une série de D/R

parseInput :: String -> Cavern
parseInput raw =
    fromList . concat $
    indexedMap
        (\(y, row) ->
            indexedMap
                (\(x, cell) ->
                    ((x,y), read [cell])
                )
                row
        )
        (lines raw)

indexedMap :: ((Int, a) -> b) -> [a] -> [b]
indexedMap f l =
    go 0 f l
    where
        go :: Int -> ((Int, a) -> b) -> [a] -> [b]
        go _ _ [] = []
        go i fun (x:xs) =
            fun (i,x) : go (i + 1) fun xs


allPaths :: Cavern -> [[Direction]]
allPaths cavern =
    let
        x = foldrWithKey (\(x,y) _ acc -> max acc x) 0 cavern
        y = foldrWithKey (\(x,y) _ acc -> max acc y) 0 cavern
        -- x = maximum . fst . (Data.Map.Strict.map fst) $ cavern
        -- y = maximum . snd . (Data.Map.Strict.map fst) $ cavern
    in
    nub . permutations $ (replicate (x - 1) R) ++ (replicate (y - 1) D)

dimensions :: Cavern -> (Int, Int)
dimensions cavern =
    (x + 1, y + 1)
    where
        x = foldrWithKey (\(x,y) _ acc -> max acc x) 0 cavern
        y = foldrWithKey (\(x,y) _ acc -> max acc y) 0 cavern


firstPath :: Cavern -> [Direction]
firstPath cavern =
    let
        (x, y) = dimensions cavern
    in
    replicate (x - 1) R ++ replicate (y - 1) D

riskLevel :: [Direction] -> Cavern -> Int
riskLevel path cavern =
    go path cavern 0 (0,0)
    where
        go :: [Direction] -> Cavern -> Int -> (Int, Int) -> Int
        go [] _ totalRisk _ = totalRisk
        go (dir:rest) cavern_ totalRisk (oldX, oldY) =
            let
                newPos = case dir of
                    R -> (oldX + 1, oldY)
                    D -> (oldX, oldY + 1)

                risk = fromJust $ Data.Map.Strict.lookup newPos cavern_
            in
            go rest cavern_ (totalRisk + risk) newPos


main :: IO ()
main = do
    raw <- getContents
    let cavern = parseInput raw
    print $ riskLevel (firstPath cavern) cavern