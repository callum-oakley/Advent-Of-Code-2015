-- Part 1
import Data.List

lookAndSay :: String -> String
lookAndSay = concatMap (\x -> (show . length $ x) ++ [head x]) . group

lengthOfResult :: Int -> String -> Int
lengthOfResult n = length . (!! n) . iterate lookAndSay

-- lengthOfResult 40 "1321131112"

-- Part 2
-- lengthOfResult 50 "1321131112"
