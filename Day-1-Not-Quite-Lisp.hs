whatFloor :: String -> Int
whatFloor = sum . map (\p -> if p == '(' then 1 else -1)
