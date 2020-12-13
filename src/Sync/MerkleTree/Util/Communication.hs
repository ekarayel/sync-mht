module Sync.MerkleTree.Util.Communication
  ( send,
    receive,
  )
where

import qualified Data.ByteString as BS
import Data.Bytes.Put (runPutS)
import Data.Bytes.Serial (Serial, deserialize, serialize)
import qualified Data.Serialize as CES
import System.IO.Streams (InputStream, OutputStream, read, unRead, write)
import Prelude hiding (read)

-- | Deserialize value from inputstream
receive :: (Serial a) => InputStream BS.ByteString -> IO a
receive input = go (CES.Partial $ CES.runGetPartial deserialize)
  where
    go (CES.Fail err _bs) = fail err
    go (CES.Partial f) =
      do
        x <- read input
        case x of
          Nothing -> (go $ f BS.empty)
          Just x'
            | BS.null x' -> go (CES.Partial f)
            | otherwise -> go (f x')
    go (CES.Done r bs) = unRead bs input >> return r

-- | Serialize value to OutputStream
send :: (Serial a) => OutputStream BS.ByteString -> a -> IO ()
send out msg = do
  write (Just $ runPutS $ serialize msg) out
  write (Just "") out -- flush underlying handle
