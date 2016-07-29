import qualified Data.ByteString.Lazy.Char8 as B
import System.IO
import System.Environment
import System.Console.GetOpt
import System.Exit
import Shelly
import Data.Text as T

data Flag = FApxDir String | FFramework String | FSemantics (Maybe String)
    deriving (Eq,Ord,Show)

flags = [Option ['a'] [] (ReqArg FApxDir "APX_DIR")
            "Directory containing the Aspartix ASP encodings.",
          Option ['f'] [] (ReqArg FFramework "FRAMEWORK_DIR")
            "Directory containing the the argumentation frameworks in APX format."
          --,Option ['s'] [] (ReqArg FSemantics "SEMANTCIS")
          --  "Comma seprerated list of four letter names of the semantics to calculate."
          ]

header = "Usage: semantics -a APX_DIR -f FRAMEWORK_DIR [-s SEMANTICS]\n"

getFrameworkPath [] = Nothing
getFrameworkPath (FFramework s:_) = Just s
getFrameworkPath (_:xs) = getFrameworkPath xs

getApxDirPath [] = Nothing
getApxDirPath (FApxDir s:_) = Just s
getApxDirPath (_:xs) = getApxDirPath xs

onErr e = do
  hPutStrLn stderr (concat e ++ usageInfo header flags)
  exitWith (ExitFailure 1)

readFromFile ∷ Show a ⇒ String → ([B.ByteString] → a) → IO a
readFromFile path parser = do
  handle ← openFile path ReadMode
  content ← B.hGetContents handle
  return $ (parser.B.lines) content

main ∷ IO ()
main = do
  args ← getArgs
  case getOpt Permute flags args of
    (args,_,[]) →
        case (getFrameworkPath args, getApxDirPath args) of
          (Just framework, Just extensions) → do
              onErr ["not impl"]
          _ → onErr []
    (_,_,errs) → onErr errs
