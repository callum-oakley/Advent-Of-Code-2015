-- Part 1
import Data.List

hasThreeVowels :: String -> Bool
hasThreeVowels = (> 2) . length . filter (flip elem "aeiou")

hasOffsetTwin :: Int -> Int -> String -> Bool
hasOffsetTwin subLength offset s = isInfixOf (take subLength $ repeat True)
    $ zipWith (==) (drop offset s) s

hasNoBadPairs :: String -> Bool
hasNoBadPairs s = not . any (flip isInfixOf s) $ ["ab", "cd", "pq", "xy"]

isNiceString :: [String -> Bool] -> String -> Bool
isNiceString checks s = and . map (\check -> check s) $ checks

howManyNiceStringsGeneral :: [String -> Bool] -> [String] -> Int
howManyNiceStringsGeneral checks = length . filter (isNiceString checks)

howManyNiceStringsOne :: [String] -> Int
howManyNiceStringsOne = howManyNiceStringsGeneral
    [hasThreeVowels, hasOffsetTwin 1 1, hasNoBadPairs]

-- Part 2
hasNonoverlappingTwinPair :: String -> Bool
hasNonoverlappingTwinPair s =
    any (\offset -> hasOffsetTwin 2 offset s) [2..(length s - 2)]

howManyNiceStringsTwo :: [String] -> Int
howManyNiceStringsTwo = howManyNiceStringsGeneral
    [hasNonoverlappingTwinPair, hasOffsetTwin 1 2]
