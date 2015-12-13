-- Part 1
-- originally did part one with regex in an editor but found this awesome "readMany" function on reddit that I had to make use of.
{-# LANGUAGE OverloadedStrings #-} -- so we can write "red" for Text
import Data.Aeson
import Data.List
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as C

readMany :: Read a => String -> [a]
readMany = unfoldr $ listToMaybe . concatMap reads . tails

total :: String -> Int
total = sum . readMany

-- Part 2
sumNumbers :: Value -> Int
sumNumbers (Object o)
    | elem "red" o    = 0
    | otherwise       = foldr ((+) . sumNumbers) 0 o
sumNumbers (Array a)  = foldr ((+) . sumNumbers) 0 a
sumNumbers (Number n) = truncate n
sumNumbers _          = 0

total' :: String -> Int
total' = sumNumbers . fromJust . decode . C.pack
