-- Part 1
import Data.Maybe
import Data.Random
import Data.Random.Sample
import Data.Random.Extras
import System.Random

data Effect = Effect {
    timer          :: Integer,
    parentSpell    :: String,
    action         :: (PlayerStats, BossStats) -> (PlayerStats, BossStats)
}

data Spell = Spell {
    name           :: String,
    cost           :: Integer,
    instantDamage  :: Integer,
    instantHealing :: Integer,
    effects        :: [Effect]
}

data PlayerStats = PlayerStats {
    playerHP       :: Integer,
    mana           :: Integer,
    manaSpent      :: Integer,
    magicArmor     :: Integer,
    spells         :: [Spell]
}

data BossStats = BossStats {
    bossHP         :: Integer,
    damage         :: Integer
}

data Signal = PlayerToAttack | BossToAttack | End deriving (Eq)
type State = (PlayerStats, BossStats, [Effect], Signal, StdGen)
data Difficulty = Easy | Hard

difficultyModifier :: Difficulty -> State -> State
difficultyModifier Hard (p, b, es, PlayerToAttack, r) =
    (p {playerHP = playerHP p - 1}, b, es, PlayerToAttack, r)
difficultyModifier _ state = state

testForWin :: State -> State
testForWin (p, b, es, s, r)
    | playerHP p <= 0 || bossHP b <= 0 = (p, b, es, End, r)
    | otherwise                        = (p, b, es, s, r)

applyEffects :: State -> State
applyEffects (p, b, es, s, r)   = (p', b', es', s, r)
  where
    (p', b') = foldr ($) (p, b) . map action $ es
    es' = filter ((> 0) . timer) . map (\e -> e {timer = timer e - 1}) $ es

chooseSpell :: [Spell] -> StdGen -> (Spell, StdGen)
chooseSpell spells = sampleState (choice spells)

invoke :: Spell -> (PlayerStats, BossStats, [Effect]) ->
    (PlayerStats, BossStats, [Effect])
invoke spell (p, b, es) = (p', b', es')
  where
    p' = p {playerHP  = playerHP p + instantHealing spell,
            mana      = mana p - cost spell,
            manaSpent = manaSpent p + cost spell}
    b' = b {bossHP    = bossHP b - instantDamage spell}
    es' = es ++ effects spell

attack :: State -> State
attack (p, b, es, End, r) = (p, b, es, End, r)
attack (p, b, es, PlayerToAttack, r)
    | null validSpells = (p {playerHP = 0}, b, es, End, r)
    | otherwise        = (p', b', es', BossToAttack, r')
  where
    validSpells = filter
        (\s -> mana p - cost s >= 0 && not (elem (name s) (map parentSpell es)))
        $ spells p
    (spell, r') = chooseSpell validSpells r
    (p', b', es') = invoke spell (p, b, es)
attack (p, b, es, BossToAttack, r) = (p', b, es, PlayerToAttack, r)
  where
    p' = p {playerHP = playerHP p - max 1 (damage b - magicArmor p)}

resetMagicArmor :: State -> State
resetMagicArmor (p, b, es, s, r) = (p {magicArmor = 0}, b, es, s, r)

fight :: Difficulty -> State -> State
fight difficulty (p, b, es, End, r) = (p, b, es, End, r)
fight difficulty state =
    (fight difficulty) . resetMagicArmor . testForWin . attack . testForWin .
    applyEffects . testForWin . difficultyModifier difficulty $ state

playerWin :: State -> Bool
playerWin (_, b, _, _, _) = bossHP b <= 0

finalManaSpent :: State -> Integer
finalManaSpent (p, _, _, _, _) = manaSpent p

leastManaToWin :: Difficulty -> (StdGen -> State) -> Int -> Integer
leastManaToWin difficulty stateGenerator n = minimum . map finalManaSpent .
    filter playerWin . map ((fight difficulty) . stateGenerator . mkStdGen) $
    [0..n]

-- Part 2
-- Just set difficulty to Hard.
