-- Part 1
import Data.Array
import Data.List

type Person = Int
type Happiness = Int
type SeatingPlan = [Person]
type HappinessArray = Array (Person, Person) Happiness

cycles :: [a] -> [[a]]
cycles (x:xs) = map ((x:) . (++ [x])) $ permutations xs

rotate :: [a] -> [a]
rotate [] = []
rotate (x:xs) = xs ++ [x]

maxHappiness :: HappinessArray -> Happiness
maxHappiness happinessArray = maximum . map happiness . cycles $ [0..n]
  where
    n                 = fst . snd . bounds $ happinessArray
    pairHappiness p q = happinessArray ! (p, q) + happinessArray ! (q, p)
    happiness seating = sum $ zipWith pairHappiness seating (rotate seating)
