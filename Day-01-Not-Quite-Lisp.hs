-- Part 1
parensToInts :: String -> [Int]
parensToInts = map (\p -> if p == '(' then 1 else -1)

whatFloor :: String -> Int
whatFloor = sum . parensToInts

-- Part 2
whatPosition :: String -> Int
whatPosition = length . takeWhile (>= 0) . scanl (+) 0 . parensToInts
