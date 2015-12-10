-- Part 1
import Data.List

look :: String -> [(Int, Char)]
look (x:xs) = reverse . foldl' countConsecutiveChars [(1, x)] $ xs
  where
    countConsecutiveChars acc@((n, y):ys) z
        | y == z    = (n + 1, y):ys
        | otherwise = (1, z):acc

say :: [(Int, Char)] -> String
say = concatMap (\(n, x) -> (show n) ++ [x])

lookAndSay :: String -> [String]
lookAndSay = iterate (say . look)

lengthOfResult :: String -> Int -> Int
lengthOfResult seed n = length . head . reverse . take n . lookAndSay $ seed

-- lengthOfResult "1321131112" 41
