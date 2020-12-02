import Data.Version (showVersion)
import Paths_sync_mht (version)
import System.Environment
import qualified Sync.MerkleTree.Run as S

main :: IO ()
main = getArgs >>= S.main (showVersion version)
