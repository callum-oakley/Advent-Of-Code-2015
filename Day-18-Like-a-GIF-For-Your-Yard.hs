-- Part 1
import Data.Array
import Data.List

data Light = On | Off deriving (Eq, Show, Read)
type LightIndex = (Int, Int)
type LightGrid = Array LightIndex Light

simulateStep :: [LightIndex] -> LightGrid -> LightGrid
simulateStep fixedOn grid = array ((0,0),(101,101))
    [((i,j), light i j) | i <- [0..101], j <- [0..101]]
  where
    neighborsOn i j = length . filter (== On) .
        map (\(di, dj) -> grid ! (i + di, j + dj)) $
        [(-1,-1),(-1,0),(-1,1),(0,-1),(0,1),(1,-1),(1,0),(1,1)]
    light i j
        | intersect [i, j] [0, 101] /= [] = Off
        | elem (i, j) fixedOn             = On
        | grid ! (i,j) == On = if neighborsOn i j `elem` [2, 3] then On else Off
        | otherwise          = if neighborsOn i j == 3          then On else Off

simulateSteps :: Int -> [LightIndex] -> LightGrid -> LightGrid
simulateSteps n fixedOn = foldr (.) id (replicate n (simulateStep fixedOn))

lightsOn :: Int -> LightGrid -> Int
lightsOn n = length . filter (== On) . elems . simulateSteps n []

-- Part 2
lightsOn' :: Int -> LightGrid -> Int
lightsOn' n = length . filter (== On) . elems .
    simulateSteps n [(1,1),(1,100),(100,1),(100,100)]
