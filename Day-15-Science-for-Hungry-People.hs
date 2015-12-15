-- Part 1
import Data.List
import Math.Combinat.Partitions.Integer

type Ingredient = [Int]
type Recipe     = [Int]

recipes :: Int -> [Recipe]
recipes n = concatMap (permutations . pad) $ _partitions' (100, n) 100
  where
    pad p = (p ++) $ replicate (n - length p) 0

score :: (Int -> Int -> Int) -> [Ingredient] -> Recipe -> Int
score f ingredients recipe = calorieTransformation . product . map (max 0) .
    init $ cookieProperties
  where
    cookieProperties = foldr1 (zipWith (+)) $
        zipWith (\i m -> map (* m) i) ingredients recipe
    calorieTransformation = f . last $ cookieProperties

maxScore :: (Int -> Int -> Int) -> [Ingredient] -> Int
maxScore f ingredients = maximum . map (score f ingredients) . recipes .
    length $ ingredients

maxScoreIgnoringCalories :: [Ingredient] -> Int
maxScoreIgnoringCalories = maxScore (const id)

-- Part 2
maxScoreWithFixedCalories :: Int -> [Ingredient] -> Int
maxScoreWithFixedCalories n = maxScore (\x -> if x == n then id else const 0)
