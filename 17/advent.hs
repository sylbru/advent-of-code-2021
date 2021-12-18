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

tryVysForTargetWithVx :: Target -> Int -> [(Velocity, Int)]
tryVysForTargetWithVx target vx =
    let
        minVy = fst $ snd target
        maxVy = 200
    in
    mapMaybe (\vy -> checkForTarget (vx,vy) target) [minVy..maxVy]

allHeights :: Target -> [(Velocity, Int)]
allHeights target =
    let
        minVx = head [ vx | vx <- [1..], (vx * (vx + 1)) `div` 2 >= (fst $ fst target) ]
        maxVx = snd $ fst target
    in
    concat $ map (tryVysForTargetWithVx target) [minVx..maxVx]

main :: IO ()
main = do
    let allOfThem = allHeights input
    -- print . maximum . map (\(_,h) -> h) $ allOfThem
    print . length $ allOfThem