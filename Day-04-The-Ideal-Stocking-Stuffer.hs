-- Part 1
import Crypto.Hash
import Data.List
import qualified Data.ByteString.Lazy.Char8 as B

md5 :: String -> Digest MD5
md5 = hashlazy . B.pack

searchHashes :: String -> String -> Int
searchHashes prefix key = head . filter p $ [1..]
  where
    p = isPrefixOf prefix . show . md5 . (key ++) . show

find5zeros :: String -> Int
find5zeros = searchHashes "00000"

-- Part 2
find6zeros :: String -> Int
find6zeros = searchHashes "000000"
