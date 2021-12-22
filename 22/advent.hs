import Data.Char (isSpace)
import Data.List
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Set as Set

type Instruction = (State, Cuboid)
type Cuboid = ((Int, Int), (Int, Int), (Int, Int))
--            ((x1, x2), (y1, y2), (z1, z2))
data State = On | Off deriving (Eq, Show)
type ReactorCore = Set (Int, Int, Int)


parseInput :: String -> [Instruction]
parseInput input =
    map parseInstruction $ lines input

parseInstruction :: String -> Instruction
parseInstruction line =
    let
        (rawState, rawCoords) = listTo2Tuple $ splitOn " " line
        state = case rawState of
            "on" -> On
            "off" -> Off
        coords =
            listTo3Tuple . map listTo2Tuple . map (map read) . map (splitOn "..") . map (drop 2) . splitOn ","
                $ rawCoords
    in
    (state, coords)

applyInstructions :: [Instruction] -> ReactorCore -> ReactorCore
applyInstructions [] core = core
applyInstructions (instruction:rest) core =
    applyInstructions rest (applyInstruction instruction core)

applyInstruction :: Instruction -> ReactorCore -> ReactorCore
applyInstruction (state, cuboid) reactorCore =
    let
        cubes = toCubes cuboid
    in
    case state of
        On ->
            foldr Set.insert reactorCore cubes
        Off ->
            foldr Set.delete reactorCore cubes

toCubes :: Cuboid -> [(Int, Int, Int)]
toCubes ((x1, x2), (y1, y2), (z1, z2)) =
    map listTo3Tuple $ concatMap (\x -> concatMap (\y -> map (\z -> [x,y,z]) [z1..z2]) [y1..y2]) [x1..x2]

listTo2Tuple :: [a] -> (a, a)
listTo2Tuple (a1:a2:_) = (a1, a2)

listTo3Tuple :: [a] -> (a, a, a)
listTo3Tuple (a1:a2:a3:_) = (a1, a2, a3)

main :: IO ()
main = do
    raw <- getContents
    let instructions = parseInput raw
    let initial = Set.empty
    print . Set.size $ applyInstructions instructions initial
