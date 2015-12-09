-- Part 1
import Data.Array
import Data.List

type City = Int
type Distance = Int
type DistanceArray = Array (City, City) Distance

cycles :: [a] -> [[a]]
cycles (x:xs) = map (x:) $ permutations xs

rotate :: [a] -> [a]
rotate [] = []
rotate (x:xs) = xs ++ [x]

extremalRoute :: (Distance -> Distance -> Ordering) -> ([City] -> City) ->
    DistanceArray -> Distance
extremalRoute comparison reduction distances =
    reduction . map totalDistance . cycles $ [0..n]
  where
    n = fst . snd . bounds $ distances
    totalDistance cities = sum . tail . sortBy comparison $
        zipWith (\x y -> distances ! (x, y)) cities (rotate cities)

shortestRoute :: DistanceArray -> Distance
shortestRoute = extremalRoute (flip compare) minimum

-- Part 2
longestRoute :: DistanceArray -> Distance
longestRoute = extremalRoute compare maximum
