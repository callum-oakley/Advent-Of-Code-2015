-- Part 1
import Data.Function.Memoize
import Data.List

type Container = Int
type Eggnog = Int

eggnogCombos :: [Container] -> Eggnog -> Int
eggnogCombos = m
  where
    eggnogCombos _ 0 = 1
    eggnogCombos containers eggnog = sum . map (\(c, i) ->
        if (c > eggnog) then 0
            else (m (drop i containers)  (eggnog - c))) $ zip containers [1..]
    m = memoize eggnogCombos
