-- Part 1
import Data.List

type Container = Integer
type Eggnog = Integer

eggnogCombos :: Eggnog -> [Container] -> [[Container]]
eggnogCombos eggnog = filter ((== eggnog) . sum) . subsequences

noOfCombos :: Eggnog -> [Container] -> Int
noOfCombos eggnog = length . eggnogCombos eggnog

-- Part 2
noOfMinimalCombos :: Eggnog -> [Container] -> Int
noOfMinimalCombos eggnog =
    length . head . group . sort . map length . eggnogCombos eggnog
