-- Part 1
import Data.List

paper :: [Int] -> Int -- assumes x <= y <= z
paper [x, y, z] = 3*x*y + 2*y*z + 2*z*x

totalPaper :: [[Int]] -> Int
totalPaper = sum . map paper . map sort

-- Part 2
ribbon :: [Int] -> Int -- assumes x <= y <= z
ribbon [x, y, z] = 2*x + 2*y + x*y*z

totalRibbon :: [[Int]] -> Int
totalRibbon = sum . map ribbon . map sort
