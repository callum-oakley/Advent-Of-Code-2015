-- Part 1
import Data.List

-- we first hand process the input so that it is in the form [..[l, w, h]..]

area :: [Int] -> Int
area [x, y, z] = 3*x*y + 2*y*z + 2*z*x

totalArea :: [[Int]] -> Int
totalArea = sum . map area . map sort
