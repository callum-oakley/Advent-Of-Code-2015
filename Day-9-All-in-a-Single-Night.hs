-- Part 1
import Data.Array
import Data.List

-- process the input data in to an array

type City = Int
type Distance = Int

cycles :: [a] -> [[a]]
cycles (x:xs) = map (x:) $ permutations xs

rotate :: [a] -> [a]
rotate [] = []
rotate (x:xs) = xs ++ [x]

shortestRoute :: Array (City, City) Distance -> Distance
shortestRoute distances = minimum . map totalDistance . cycles $ [0..n]
  where
    n = fst . snd . bounds $ distances
    totalDistance cities = sum . tail . sortBy (flip compare) $
        zipWith (\x y -> distances ! (x, y)) cities (rotate cities)
