import Data.Maybe (mapMaybe)

type Position = (Int, Int)
type Velocity = (Int, Int)

--             x          y
type Target = ((Int,Int), (Int,Int))

input :: Target
input = ((207, 263), (-115, -63))

example :: Target
example = ((20, 30), (-10, -5))

checkForTarget :: Velocity -> Target -> Maybe (Velocity, Int)
checkForTarget initialVelocity target_ =
    go (0,0) initialVelocity target_ 0
    where
        go pos@(x,y) velocity@(vx,vy) target@((x1,x2),(y1,y2)) highestY =
            if x1 <= x && x <= x2 && y1 <= y && y <= y2 then
                Just (initialVelocity, highestY)
            else
                if vx /= 0 || (x >= x1 && x <= x2 && y >= y1) then
                    let
                        (newPos, newVelocity) = step pos velocity
                    in
                    go newPos newVelocity target (max (snd newPos) highestY)
                else
                    Nothing

step :: Position -> Velocity -> (Position, Velocity)
step (x,y) (vx,vy) =
    ( (x + vx, y + vy)
    , ( if vx < 0 then vx + 1 else if vx > 0 then vx - 1 else vx
      , vy - 1
      )
    )

vxsForTarget :: Target -> [Int]
vxsForTarget ((x1, x2), _) =
    [ v | v <- [0..200]
        , (v * (v + 1)) `div` 2 >= x1
        && (v * (v + 1)) `div` 2 <= x2
    ]

tryVysForTargetWithVx :: Target -> Int -> [(Velocity, Int)]
tryVysForTargetWithVx target vx =
    mapMaybe (\vy -> checkForTarget (vx,vy) target) [-200..200]

allHeights :: Target -> [(Velocity, Int)]
allHeights target =
    concat $ map (tryVysForTargetWithVx target) [-300..300]

main :: IO ()
main = do
    let allOfThem = allHeights input
    print . maximum . map (\(_,h) -> h) $ allOfThem
    print . length . map (\(v,_) -> v) $ allOfThem