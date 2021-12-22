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
data State = On | Off deriving (Eq, Show)
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
    (Cuboid, coords)

applyInstructions :: [Instruction] -> ReactorCore -> ReactorCore
applyInstructions [] core = core
applyInstructions (instruction:rest) core =
    applyInstructions rest (applyInstruction instruction core)

-- applyInstruction :: Instruction -> ReactorCore -> ReactorCore
-- applyInstruction cuboid reactorCore =
--     insertCuboid cuboid
--     case cuboidType of
--         Cuboid -> addCuboid cuboid reactorCore
--         Off -> subtractCuboid cuboid reactorCore

-- subtractCuboid :: Cuboid -> Set Cuboid -> Set Cuboid
-- subtractCuboid (AntiCuboid, _) cuboids = cuboids
-- subtractCuboid (Cuboid, cuboidCoords) cuboids =
--     insertCuboid (AntiCuboid, cuboidCoords) cuboids

-- addCuboid :: Cuboid -> Set Cuboid -> Set Cuboid
-- addCuboid (AntiCuboid, _) cuboids = cuboids
-- addCuboid (Cuboid, cuboidCoords) cuboids =
--     insertCuboid (Cuboid, cuboidCoords) cuboids

applyInstruction :: Cuboid -> Set Cuboid -> Set Cuboid
applyInstruction cuboid@(cuboidType, cuboidCoords) cuboids =
    let
        withCuboid = Set.insert cuboid cuboids
    in
    withCuboid

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
    print . countTotalCubes $ applyInstructions instructions Set.empty
