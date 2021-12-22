import Data.Char (isSpace)
import Data.List
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Set as Set

type Instruction = Cuboid
type Cuboid = (CuboidType, ((Int, Int), (Int, Int), (Int, Int)))
--            ((x1, x2), (y1, y2), (z1, z2))
data CuboidType = Cuboid | AntiCuboid deriving (Eq, Show, Ord)
type ReactorCore = Set Cuboid



parseInput :: String -> [Instruction]
parseInput input =
    map parseInstruction $ lines input

parseInstruction :: String -> Instruction
parseInstruction line =
    let
        (rawState, rawCoords) = listTo2Tuple $ splitOn " " line
        cuboidType = case rawState of
            "on" -> Cuboid
            "off" -> AntiCuboid
        coords =
            listTo3Tuple . map listTo2Tuple . map (map read) . map (splitOn "..") . map (drop 2) . splitOn ","
                $ rawCoords
    in
    (cuboidType, coords)

applyInstructions :: [Instruction] -> ReactorCore -> ReactorCore
applyInstructions [] core = core
applyInstructions (instruction:rest) core =
    applyInstructions rest (applyInstruction instruction core)

applyInstruction :: Cuboid -> Set Cuboid -> Set Cuboid
applyInstruction cuboid cuboids =
    let
        intersections = findIntersections cuboid cuboids
        compensatingCuboids =
            map
                (\intersection@(intersectingType, intersectingCoords) ->
                    (inverseCuboidType intersectingType, intersectingCoords)
                )
                intersections
    in
    case fst cuboid of
        Cuboid ->
            insertSeveral (cuboid:compensatingCuboids) cuboids
        AntiCuboid ->
            insertSeveral compensatingCuboids cuboids
            -- insertSeveral (map (\(t, c) -> (inverseCuboidType t, c)) compensatingCuboids) cuboids

findIntersections :: Cuboid -> Set Cuboid -> [Cuboid]
findIntersections (_, ((x1, x2), (y1, y2), (z1, z2))) cuboids =
    Set.foldr checkIntersection [] cuboids
    where
        checkIntersection :: Cuboid -> [Cuboid] -> [Cuboid]
        checkIntersection (cuboidType_, ((x1', x2'), (y1', y2'), (z1', z2'))) intersections =
            if (x1' <= x2 && x2' >= x1)
                && (y1' <= y2 && y2' >= y1)
                && (z1' <= z2 && z2' >= z1) then
                -- 1. should both types be the same? what about intersecting a cuboid with an anticuboid?
                -- 2. maybe we should check for intersections at every intersecting cuboid we add?
                ( cuboidType_
                , ( (max x1' x1, min x2' x2)
                  , (max y1' y1, min y2' y2)
                  , (max z1' z1, min z2' z2)
                  )
                ) : intersections
            else
                intersections


insertSeveral :: [Cuboid] -> Set Cuboid -> Set Cuboid
insertSeveral [] cuboids = cuboids
insertSeveral (cuboid:rest) cuboids = insertSeveral rest (Set.insert cuboid cuboids)

inverseCuboidType :: CuboidType -> CuboidType
inverseCuboidType cuboidType =
    case cuboidType of
        Cuboid -> AntiCuboid
        AntiCuboid -> Cuboid

countCubes :: Cuboid -> Int
countCubes (cuboidType, ((x1, x2), (y1, y2), (z1, z2))) =
    sign * abs ((1 + x2 - x1) * (1 + y2 - y1) * (1 + z2 - z1))
    where
        sign =
            case cuboidType of
                Cuboid -> 1
                AntiCuboid -> -1

countTotalCubes :: ReactorCore -> Int
countTotalCubes reactorCore =
    sum . map countCubes . Set.elems $ reactorCore

listTo2Tuple :: [a] -> (a, a)
listTo2Tuple (a1:a2:_) = (a1, a2)

listTo3Tuple :: [a] -> (a, a, a)
listTo3Tuple (a1:a2:a3:_) = (a1, a2, a3)

main :: IO ()
main = do
    raw <- getContents
    let instructions = parseInput raw
    let reactor = applyInstructions instructions Set.empty
    print reactor
    print $ countTotalCubes reactor
