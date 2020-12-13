import qualified Sync.MerkleTree.Run as S
import System.Environment

main :: IO ()
main = getArgs >>= S.main
