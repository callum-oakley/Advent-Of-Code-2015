-- Part 1
import Data.List
import Math.Combinat.Partitions.Integer

type Ingredient = [Int]

combinations :: Int -> [[Int]]
combinations n = concatMap (permutations . pad) $ _partitions' (100, n) 100
  where
    pad p = (p ++) $ replicate (n - length p) 0

maxScore :: [Ingredient] -> Int
maxScore ingredients = maximum . map score . combinations . length $ ingredients
  where
      multiply ingredient multiplier = map (* multiplier) ingredient
      score = product . map (max 0) . foldr1 (zipWith (+)) .
        zipWith multiply ingredients
