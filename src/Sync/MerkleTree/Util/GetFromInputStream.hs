module Sync.MerkleTree.Util.GetFromInputStream
    ( getFromInputStream
    ) where

import qualified Data.ByteString as BS
import qualified Data.Bytes.Serial as SE
import qualified Data.Serialize as CES
import qualified System.IO.Streams as ST

-- | Deserialize value from inputstream
getFromInputStream :: (SE.Serial a) => ST.InputStream BS.ByteString -> IO a
getFromInputStream s = go (CES.Partial $ CES.runGetPartial SE.deserialize)
    where
      go (CES.Fail err _bs) = fail err
      go (CES.Partial f) =
          do x <- ST.read s
             case x of
               Nothing -> (go $ f BS.empty)
               Just x' | BS.null x' -> go (CES.Partial f)
                       | otherwise -> go (f x')
      go (CES.Done r bs) = ST.unRead bs s >> return r
