-- Part 1
import Data.List
import Data.Maybe
import qualified Data.Map.Lazy as Map

type Sue = Map.Map String Int

matchingSueGeneral :: (Sue -> Sue -> Bool) -> Sue -> [Sue] -> Int
matchingSueGeneral p tape = succ . fromJust . findIndex (p tape)

matchingSue :: Sue -> [Sue] -> Int
matchingSue = matchingSueGeneral (flip Map.isSubmapOf)

-- Part 2
isMatching :: Sue -> Sue -> Bool
isMatching tape sue = and . map (\(op, w) -> Map.isSubmapOfBy op w tape) $
    [((<), xs), ((>), ys), ((==), zs)]
  where
    (xs, remaining) =
        Map.partitionWithKey (\k _ -> elem k ["pomeranians", "goldfish"]) sue
    (ys, zs) =
        Map.partitionWithKey (\k _ -> elem k ["cats", "trees"]) remaining

matchingSue' :: Sue -> [Sue] -> Int
matchingSue' = matchingSueGeneral isMatching
