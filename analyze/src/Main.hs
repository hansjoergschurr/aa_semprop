import Frameworks
import qualified Data.ByteString.Lazy.Char8 as B
import System.IO

main ∷ IO ()
main = do
  let filename = "test.tgf"
  handle ← openFile filename ReadMode
  content ←  B.hGetContents handle
  let framework = readTgf $ B.lines content
  print framework
