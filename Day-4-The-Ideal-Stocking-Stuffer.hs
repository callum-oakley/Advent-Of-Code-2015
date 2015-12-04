import Crypto.Hash
import qualified Data.ByteString.Lazy.Char8 as B

md5 :: String -> Digest MD5
md5 = hashlazy . B.pack

naive :: String -> Int
naive key = head . filter p $ [1..]
  where
    p = (== "00000") . take 5 . show . md5 . (key ++) . show
