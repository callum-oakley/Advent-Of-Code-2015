-- Part 1
import Data.List

inc :: Char -> (Bool, String) -> (Bool, String)
inc x (carry, xs)
    | not carry    = (False, x:xs)
    | x == 'z'     = (True, 'a':xs)
    | elem x "hnk" = (False, (succ . succ $ x):xs)
    | otherwise    = (False, (succ x):xs)

increment :: String -> String
increment = snd . foldr inc (True, [])

hasStraight :: String -> Bool
hasStraight = any ((>= 3) . length) . group . zipWith ($)
    (scanl' (.) id (repeat pred))

hasPairs :: String -> Bool
hasPairs = (>= 2) . length . filter ((>= 2) . length) . group

isValid :: String -> Bool
isValid s = hasPairs s && hasStraight s

findNextValid :: String -> String -- assumes no 'i' 'o' or 'l' in input
findNextValid = head . filter isValid . tail . iterate increment

-- Part 2
-- Just feed the output from part 1 back in to findNextValid once more.
