-- Part 1
import Data.List

hasThreeVowels :: String -> Bool
hasThreeVowels = (> 2) . length . filter isVowel
  where
    isVowel c = elem c "aeiou"

hasDouble :: String -> Bool
hasDouble s = (/= []) . filter isDiagonal $ zip (tail s) s
  where
    isDiagonal (x, y) = x == y

hasNoBadPairs :: String -> Bool
hasNoBadPairs s = (== []) . filter (flip isInfixOf s) $ badPairs
  where
    badPairs = ["ab", "cd", "pq", "xy"]

isNiceString :: String -> Bool
isNiceString s = and . map (\check -> check s) $ checks
  where
    checks = [hasThreeVowels, hasDouble, hasNoBadPairs]

howManyNiceStrings :: [String] -> Int
howManyNiceStrings = length . filter isNiceString
