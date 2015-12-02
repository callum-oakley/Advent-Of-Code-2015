-- Part 1
import Data.List

-- we first hand process the input so that it is in the form [..[l, w, h]..]

paper :: [Int] -> Int
paper [x, y, z] = 3*x*y + 2*y*z + 2*z*x

totalPaper :: [[Int]] -> Int
totalPaper = sum . map paper . map sort

-- Part 2
ribbon :: [Int] -> Int
ribbon [x, y, z] = 2*x + 2*y + x*y*z

totalRibbon :: [[Int]] -> Int
totalRibbon = sum . map ribbon . map sort
