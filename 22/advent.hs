import Data.Char (isSpace)
import Data.List
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)

type Instruction = Cuboid
type Cuboid = (CuboidType, ((Int, Int), (Int, Int), (Int, Int)))
           -- ((x1, x2), (y1, y2), (z1, z2))
data CuboidType = Cuboid | AntiCuboid deriving (Eq, Show, Ord)
type ReactorCore = [Cuboid]

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

applyInstruction :: Cuboid -> [Cuboid] -> [Cuboid]
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

findIntersections :: Cuboid -> [Cuboid] -> [Cuboid]
findIntersections (_, ((x1, x2), (y1, y2), (z1, z2))) cuboids =
    foldr checkIntersection [] cuboids
    where
        checkIntersection :: Cuboid -> [Cuboid] -> [Cuboid]
        checkIntersection (cuboidType_, ((x1', x2'), (y1', y2'), (z1', z2'))) intersections =
            if (x1' <= x2 && x2' >= x1)
                && (y1' <= y2 && y2' >= y1)
                && (z1' <= z2 && z2' >= z1) then
                ( cuboidType_
                , ( (max x1' x1, min x2' x2)
                  , (max y1' y1, min y2' y2)
                  , (max z1' z1, min z2' z2)
                  )
                ) : intersections
            else
                intersections

insertSeveral :: [Cuboid] -> [Cuboid] -> [Cuboid]
insertSeveral [] cuboids = cuboids
insertSeveral (cuboid:rest) cuboids = insertSeveral rest (cuboid:cuboids)

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
    sum . map countCubes $ reactorCore

listTo2Tuple :: [a] -> (a, a)
listTo2Tuple (a1:a2:_) = (a1, a2)

listTo3Tuple :: [a] -> (a, a, a)
listTo3Tuple (a1:a2:a3:_) = (a1, a2, a3)

main :: IO ()
main = do
    raw <- getContents
    let instructions = parseInput raw
    let reactor = applyInstructions instructions []
    print $ countTotalCubes reactor
