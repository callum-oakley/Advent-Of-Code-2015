-- Part 1
import Math.NumberTheory.Primes.Factorisation

firstHouseGeneral :: Integer -> (Integer -> Integer) -> Integer -> Integer
firstHouseGeneral k visits n = head . filter ((>= n) . (*k) . visits) $ [1..]

firstHouse :: Integer -> Integer
firstHouse = firstHouseGeneral 10 divisorSum

-- Part 2
firstHouse' :: Integer -> Integer
firstHouse' = firstHouseGeneral 11
    (sum . (\h -> map (div h) . filter ((== 0) . (mod h)) $ [1..50]))
