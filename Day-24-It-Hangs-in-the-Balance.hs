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
