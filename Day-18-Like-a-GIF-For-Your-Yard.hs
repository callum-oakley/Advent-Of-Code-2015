-- Part 1
import Data.Array
import Data.List

data Light = On | Off deriving (Eq, Show, Read)
type LightGrid = Array (Int, Int) Light

update :: Ix i => (e -> e) -> Array i e -> [i] -> Array i e
update f a indices = accum (flip ($)) a (zip indices (repeat f))

simulateStep :: LightGrid -> LightGrid
simulateStep grid = array ((0,0),(101,101))
    [((i,j), light i j) | i <- [0..101], j <- [0..101]]
  where
    neighborsOn i j = length . filter (== On) .
        map (\(di, dj) -> grid ! (i + di, j + dj)) $
        [(-1,-1),(-1,0),(-1,1),(0,-1),(0,1),(1,-1),(1,0),(1,1)]
    light i j
        | intersect [i, j] [0, 101] /= [] = Off
        | grid ! (i,j) == On = if neighborsOn i j `elem` [2, 3] then On else Off
        | otherwise          = if neighborsOn i j == 3          then On else Off

simulateSteps :: Int -> LightGrid -> LightGrid
simulateSteps n = foldr (.) id (replicate n simulateStep)

lightsOn :: Int -> LightGrid -> Int
lightsOn n = length . filter (== On) . elems . simulateSteps n
