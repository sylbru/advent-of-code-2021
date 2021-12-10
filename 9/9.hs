import Data.List
import Data.Maybe

type Point = Int
type Heightmap = [[Point]]

parseInput :: String -> Heightmap
parseInput =
    map (map (read . (:[]))) . lines

lowPoints :: Heightmap -> [Point]
lowPoints heightmap =
    map fst $ filter isLowPoint (pointsWithNeighbours heightmap)
    where
        isLowPoint :: (Point, [Point]) -> Bool
        isLowPoint (point, neighbours) =
            all (> point) neighbours

pointsWithNeighbours :: Heightmap -> [(Point, [Point])]
pointsWithNeighbours heightmap =
    let
        adjAfter row = (map Just $ drop 1 row) ++ [Nothing]
        adjBefore row =  Nothing : (map Just $ init row)
        adjHoriz hm =
            zip (concat . map adjBefore $ hm) (concat . map adjAfter $ hm)
        adjVert hm =
            zip (concat . transpose . map adjAfter . transpose $ hm) (concat . transpose . map adjBefore . transpose $ hm)
    in
    zipWith3
        (\p (l,r) (u,d) ->
            (p, map fromJust . filter isJust $ [l,r,u,d])
        )
        (concat heightmap) (adjHoriz heightmap) (adjVert heightmap)

riskLevel :: Point -> Int
riskLevel point =
    1 + point

sumRiskLevelsAtLowPoints :: Heightmap -> Int
sumRiskLevelsAtLowPoints heightmap =
    sum . map riskLevel . lowPoints $ heightmap

main :: IO ()
main = do
    raw <- getContents
    print . sumRiskLevelsAtLowPoints . parseInput $ raw