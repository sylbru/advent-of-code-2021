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

toRelevantCuboid :: Cuboid -> Maybe Cuboid
toRelevantCuboid ((x1, x2), (y1, y2), (z1, z2)) =
    if (constrain x1) /= (constrain x2) && (constrain y1) /= (constrain y2) && (constrain z1) /= (constrain z2) then
        Just ((constrain x1, constrain x2), (constrain y1, constrain y2), (constrain z1, constrain z2))
    else
        Nothing
    where
        constrain :: Int -> Int
        constrain value =
            if value < -50 then -50
            else if value > 50 then 50
            else value

applyInstruction :: Instruction -> ReactorCore -> ReactorCore
applyInstruction (state, cuboid) reactorCore =
    let
        cubes = case toRelevantCuboid cuboid of
            Just relevantCuboid -> toCubes relevantCuboid
            Nothing -> []
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
    print . Set.size $ applyInstructions instructions Set.empty
