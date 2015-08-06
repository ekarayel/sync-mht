module Sync.MerkleTree.Util.GetFromInputStream
    ( getFromInputStream
    ) where

import Data.ByteString(ByteString)
import Data.Serialize(Serialize)
import System.IO.Streams(InputStream)
import qualified Data.ByteString as BS
import qualified Data.Serialize as SE
import qualified System.IO.Streams as ST

-- | Deserialize value from inputstream
getFromInputStream :: (Serialize a) => InputStream ByteString -> IO a
getFromInputStream s = go (SE.Partial $ SE.runGetPartial SE.get)
    where
      go (SE.Fail err _bs) = fail err
      go (SE.Partial f) =
          do x <- ST.read s
             case x of
               Nothing -> (go $ f BS.empty)
               Just x' | BS.null x' -> go (SE.Partial f)
                       | otherwise -> go (f x')
      go (SE.Done r bs) = ST.unRead bs s >> return r
