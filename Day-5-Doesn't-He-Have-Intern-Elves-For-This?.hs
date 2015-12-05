-- Part 1
import Data.List

hasThreeVowels :: String -> Bool
hasThreeVowels = (> 2) . length . filter isVowel
  where
    isVowel c = elem c "aeiou"

hasOffsetTwin :: Int -> Int -> String -> Bool
hasOffsetTwin subLength offset s =
    isInfixOf test . map isDiagonal $ zip (drop offset s) s
  where
    test              = take subLength $ repeat True
    isDiagonal (x, y) = x == y

hasNoBadPairs :: String -> Bool
hasNoBadPairs s = (== []) . filter (flip isInfixOf s) $ badPairs
  where
    badPairs = ["ab", "cd", "pq", "xy"]

isNiceString :: [String -> Bool] -> String -> Bool
isNiceString checks s = and . map (\check -> check s) $ checks

howManyNiceStringsGeneral :: [String -> Bool] -> [String] -> Int
howManyNiceStringsGeneral checks = length . filter (isNiceString checks)

howManyNiceStringsOne :: [String] -> Int
howManyNiceStringsOne = howManyNiceStringsGeneral
    [hasThreeVowels, hasOffsetTwin 1 1, hasNoBadPairs]

-- Part 2
hasNonoverlappingTwinPair :: String -> Bool
hasNonoverlappingTwinPair s = (/= []) . filter p $ [2..(length s - 2)]
  where
    p offset = hasOffsetTwin 2 offset s

howManyNiceStringsTwo :: [String] -> Int
howManyNiceStringsTwo = howManyNiceStringsGeneral
    [hasNonoverlappingTwinPair, hasOffsetTwin 1 2]
