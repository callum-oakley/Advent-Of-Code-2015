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
