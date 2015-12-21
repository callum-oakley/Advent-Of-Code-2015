import Math.NumberTheory.Primes.Factorisation

firstHouse :: Integer -> Integer
firstHouse n = head . filter ((>= n) .  (*10) . divisorSum) $ [1..]
