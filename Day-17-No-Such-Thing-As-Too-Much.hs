-- Part 1
import Data.Function.Memoize
import Data.List

type Container = Int
type Eggnog = Int

eggnogCombos :: [Container] -> [Container] -> Eggnog -> [[Container]]
eggnogCombos = m
  where
    eggnogCombos _    used 0      = [used]
    eggnogCombos free used eggnog = concatMap (\(c, i) ->
        if (c > eggnog) then []
            else (m (drop i free) (c:used) (eggnog - c))) $ zip free [1..]
    m = memoize eggnogCombos

noOfCombos :: [Container] -> Eggnog -> Int
noOfCombos containers = length . eggnogCombos containers []

-- Part 2
noOfMinimalCombos :: [Container] -> Eggnog -> Int
noOfMinimalCombos containers =
    length . head . group . sort . map length . eggnogCombos containers []
