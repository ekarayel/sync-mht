import System.Environment
import qualified Sync.MerkleTree.Run as S

main :: IO ()
main = getArgs >>= S.main
