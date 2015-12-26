-- Part 1
triangle :: Int -> Int
triangle n = div (n * (n + 1)) 2

code :: Int -> Int
code n = foldr (.) id (replicate (n - 1) nextCode) $ 20151125
  where
    nextCode = (flip rem 33554393) . (* 252533)

codeAt :: (Int, Int) -> Int
codeAt (x, y) = code $ triangle (x + y - 2) + y
