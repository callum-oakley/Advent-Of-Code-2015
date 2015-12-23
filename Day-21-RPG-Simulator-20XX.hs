-- Part 1
import Data.List
import Data.Function

data Character = Player | Boss deriving (Read, Show, Eq)
type Cost      = Integer

data Item = Item {
cost       :: Integer,
itemDamage :: Integer,
itemArmor :: Integer
} deriving (Read, Show)

data Stats = Stats {
    character :: Character,
    hitPoints :: Integer,
    damage    :: Integer,
    armor     :: Integer
} deriving (Read, Show)

subsetsOfSizeAtMost :: Int -> [a] -> [[a]]
subsetsOfSizeAtMost 0 _      = [[]]
subsetsOfSizeAtMost _ []     = [[]]
subsetsOfSizeAtMost n (x:xs) =
    map (x:) (subsetsOfSizeAtMost (n-1) xs) ++ subsetsOfSizeAtMost n xs

generateStats :: [Item] -> [Item] -> [Item] -> [(Cost, Stats)]
generateStats weapons armor rings = sortBy (on compare fst)
    [costAndStats w a rs | w <- weapons, a <- armor, rs <- ringCombinations]
  where
    ringCombinations = subsetsOfSizeAtMost 2 rings
    costAndStats w a rs = (sum . map cost $ w:a:rs,
        Stats {
            character = Player,
            hitPoints = 100,
            damage    = sum . map itemDamage $ w:a:rs,
            armor     = sum . map itemArmor $ w:a:rs
        })

fight :: Stats -> Stats -> Character
fight attacker (Stats {character = c, hitPoints = h, damage = d, armor = a})
    | hitPoints defender <= 0 = character attacker
    | otherwise               = fight defender attacker
  where
    defender = Stats {
        character = c,
        hitPoints = h - max 1 (damage attacker - a),
        damage    = d,
        armor     = a
    }

cheapestWin :: [Item] -> [Item] -> [Item] -> Stats -> Cost
cheapestWin weapons armor rings boss =
    fst . head . filter (\(_, player) -> fight player boss == Player) $
    generateStats weapons armor rings
