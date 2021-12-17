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
type Path = [Cave]

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
    caveSystem ++ (map swap caveSystem)


explore :: Cave -> Path -> [Cave] -> CaveSystem -> [Path]
explore fromCave previousPath bannedCaves caveSystem =
    let
        newBannedCaves =
            case fromCave of
                (Big _) -> bannedCaves
                _ -> fromCave:bannedCaves

        possibleCaves =
            stepExplore fromCave previousPath newBannedCaves caveSystem

        nextCaves :: [Cave]
        nextCaves =
            map snd . filter (\(from, to) -> from == fromCave && (not $ to `elem` newBannedCaves))
                $ caveSystem
            -- concat $ map
            --     (\path ->
            --         explore (head path) path newBannedCaves caveSystem
            --     )
            --     possibleCaves

        nextPaths :: [Path]
            map (\nextCave -> nextCave:previousPath) nextCaves


    in
    if fromCave == End then
        []
    else
        nextPaths

stepExplore :: Cave -> Path -> [Cave] -> CaveSystem -> [Path]
stepExplore fromCave previousPath bannedCaves caveSystem =
    let
        nextCaves =
            map snd
                $ filter
                    (\(from, to) -> from == fromCave && (not $ to `elem` bannedCaves))
                    caveSystem
    in
    map (:previousPath) nextCaves

main :: IO ()
main =
    print . allPaths $ exampleCaveSystem