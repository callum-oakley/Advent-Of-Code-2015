-- Part 1
import Data.List
import Data.List.Split

type Replacement = (String, String)
type Molecule = String

applyReplacement :: Molecule -> Replacement -> [Molecule]
applyReplacement molecule (x, y) = map replaceAtSite [1..n]
  where
    splitMolecule   = splitOn x molecule
    n               = pred . length $ splitMolecule
    replaceAtSite i = (++ last splitMolecule) . concat $ zipWith
        (\s j -> s ++ (if j == i then y else x)) splitMolecule [1..n]

calibration :: Molecule -> [Replacement] -> Int
calibration molecule = length . nub . concatMap (applyReplacement molecule)

-- Part 2
-- Use u/askalski's observation that Rn Y Ar can be put in bijection with ( , ). (see https://www.reddit.com/r/adventofcode/comments/3xflz8/day_19_solutions/cy4etju) Then note that all mappings are of the form

-- e to a
-- a to bc
-- a to b(c)
-- a to b(c,d)
-- a to b(c,d,f)
-- where a b c d f are elements other than e ( , )

-- Not that "number of non bracket, non comma atoms minus number of commas minus number of moves == 1" (easy proof by induction)
-- so if we make each molecule one character long, delete parentheses and delete commas AND an adjacent molecules, and finally delete one more character for the electron step, then the final length of the string should be the number of steps.
