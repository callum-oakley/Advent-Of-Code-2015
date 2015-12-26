-- can make the below much nicer by first finding small subsets with the correct sum, and then seeing if the rest can be split...

-- Part 1
balancedPartition :: (Num a, Ord a) => Int -> a -> [a] -> [([a], [a], [a])]
balancedPartition _ _ [] = [([], [], [])]
balancedPartition targetSize targetWeight (x:xs) =
    concatMap (\(a, b, c) -> [(x:a, b, c), (a, x:b, c), (a, b, x:c)]) .
    filter (\(a, b, c) -> and [length a <= targetSize,
    sum a <= targetWeight, sum b <= targetWeight, sum c <= targetWeight]) $
    balancedPartition targetSize targetWeight xs

minQE :: (Num a, Ord a) => Int -> a -> [a] -> a
minQE targetSize targetWeight = product . (\(a, _, _) -> a) . head .
    filter (\(a, _, _) -> length a <= targetSize) .
    balancedPartition targetSize targetWeight

-- minQE 5 520 [1,2,3,7,11,13,17,19,23,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113]

-- Part 2
balancedPartition' :: (Num a, Ord a) => Int -> a -> [a] ->
    [([a], [a], [a], [a])]
balancedPartition' _ _ [] = [([], [], [], [])]
balancedPartition' targetSize targetWeight (x:xs) =
    concatMap (\(a, b, c, d) ->
        [(x:a, b, c, d), (a, x:b, c, d), (a, b, x:c, d), (a, b, c, x:d)]) .
    filter (\(a, b, c, d) -> and [length a <= targetSize, sum a <= targetWeight,
    sum b <= targetWeight, sum c <= targetWeight, sum d <= targetWeight]) $
    balancedPartition' targetSize targetWeight xs

minQE' :: (Num a, Ord a) => Int -> a -> [a] -> a
minQE' targetSize targetWeight = product . (\(a, _, _, _) -> a) . head .
    filter (\(a, _, _, _) -> length a <= targetSize) .
    balancedPartition' targetSize targetWeight

-- minQE' 4 390 [1,2,3,7,11,13,17,19,23,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113]
