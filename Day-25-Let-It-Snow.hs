import Math.NumberTheory.Powers

triangle :: Integer -> Integer
triangle n = div (n * (n + 1)) 2

code :: Integer -> Integer
code n = (flip mod 33554393) . (* 20151125) $ powerMod 252533 (n - 1) 33554393

codeAt :: (Integer, Integer) -> Integer
codeAt (x, y) = code $ triangle (x + y - 2) + y
