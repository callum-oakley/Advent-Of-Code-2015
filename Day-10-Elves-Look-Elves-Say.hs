-- Part 1
import Data.List

lookAndSay :: String -> [String]
lookAndSay = iterate (concatMap say . group)
  where
    say xs = (show . length $ xs) ++ [head xs]

lengthOfResult :: String -> Int -> Int
lengthOfResult seed n = length . head . reverse . take n . lookAndSay $ seed

-- lengthOfResult "1321131112" 41

-- Part 2
-- lengthOfResult "1321131112" 51
