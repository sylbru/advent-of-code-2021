import Data.Maybe (fromJust, mapMaybe)
import Data.Tuple (swap)
import Data.List
import Debug.Trace (trace, traceShow)

-- First example (has 10 paths)
example1 ="start-A\nstart-b\nA-c\nA-b\nb-d\nA-end\nb-end\n"

-- Slightly larger example (has 19 paths)
example2 = "dc-end\nHN-start\nstart-kj\ndc-start\ndc-HN\nLN-dc\nHN-end\nkj-sa\nkj-HN\nkj-dc\n"

data Cave = Start | End | Small String | Big String
    deriving (Eq, Show)

type Connection = (Cave, Cave)
type CaveSystem = [Connection]
type Path = [Connection]

exampleCaveSystem :: CaveSystem
exampleCaveSystem =
    [ -- (Start, Big "A")
    (Start, Small "b")
    -- , (Start, Small "b")
    -- , (Big "A", Small "c")
    -- , (Big "A", Small "b")
    -- , (Small "b", Small "d")
    -- , (Big "A", End)
    , (Small "b", End)
    ]


allPaths :: CaveSystem -> [Path]
allPaths caveSystem =
    let
        bidirectionalCaveSystem = makeBidirectional caveSystem
    in
    (explore Start [] [] bidirectionalCaveSystem)
    -- filter
    --     (\path -> currentCaveFromPath path == Just End)
    --     (explore Start [] [] bidirectionalCaveSystem)


makeBidirectional :: CaveSystem -> CaveSystem
makeBidirectional caveSystem =
    nub $ caveSystem ++ (map swap caveSystem)


explore :: Cave -> Path -> [Cave] -> CaveSystem -> [Path]
explore fromCave previousPath bannedCaves caveSystem =
    let
        newBannedCaves =
            case fromCave of
                (Big _) -> bannedCaves
                _ -> fromCave:bannedCaves

        possiblePaths =
            stepExplore fromCave previousPath newBannedCaves caveSystem

        nextPaths :: [Path]
        nextPaths =
            concat $ map
                (\path ->
                    explore (fromJust $ currentCaveFromPath path) path newBannedCaves caveSystem
                )
                possiblePaths
    in
    if fromCave == End then
        []
    else
        map (\path -> previousPath ++ path) nextPaths


currentCaveFromPath :: Path -> Maybe Cave
currentCaveFromPath path =
    case path of
        [] -> Nothing -- no previousPath and no validNextConnections : there is no way out
        ((_, nowAt):_) -> Just nowAt


stepExplore :: Cave -> Path -> [Cave] -> CaveSystem -> [Path]
stepExplore fromCave previousPath bannedCaves caveSystem =
    let
        validNextConnections =
            filter
                (\(from, to) -> from == fromCave && (not $ to `elem` bannedCaves))
                caveSystem
    in
    map (:previousPath) validNextConnections

main :: IO ()
main =
    print $ explore Start [] [] (makeBidirectional exampleCaveSystem)