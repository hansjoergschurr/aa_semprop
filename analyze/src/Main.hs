import qualified Data.ByteString.Lazy.Char8 as B
import System.IO
import System.Environment
import System.Console.GetOpt
import System.Exit

import Frameworks
import Extensions
import Semantics

data Flag = FTgf | FIccma | FNumeric | FFramework String | FExtensions String
    deriving (Eq,Ord,Show)

flags = [Option ['t'] [] (NoArg FTgf)
            "The framework is in Trivial Graph Format, instead of Aspartix Format.",
         Option ['c'] [] (NoArg FIccma)
            "The extensions are in ICCMA competiton format, instead of an output log of clasp.",
         Option ['n'] [] (NoArg FNumeric)
            "Output the number of elements instead of lists e.g. for implicit conflicts.",
         Option ['f'] [] (ReqArg FFramework "FILE")
            "FILE containing the framework.",
         Option ['e'] [] (ReqArg FExtensions "FILE")
            "FILE containing the extensions."]

header = "Usage: analyze [-t] [-c] [-n] -f FILE -e FILE\n"

getFrameworkPath [] = Nothing
getFrameworkPath (FFramework s:_) = Just s
getFrameworkPath (_:xs) = getFrameworkPath xs

getExtensionsPath [] = Nothing
getExtensionsPath (FExtensions s:_) = Just s
getExtensionsPath (_:xs) = getExtensionsPath xs

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
        case (getFrameworkPath args, getExtensionsPath args) of
          (Just framework, Just extensions) → do
            let numericOut = FNumeric `elem` args
            let fParser = if FTgf `elem` args then readTgf else readApx
            let eParser = if FIccma `elem` args then readIccma else readClasp

            framework ← readFromFile framework fParser
            extensions ← readFromFile extensions eParser

            case sanityCheck framework extensions of
              Just s → do
                hPutStrLn stderr ("Error: "++s)
                exitWith (ExitFailure 1)
              Nothing → outputSemanticProperties numericOut framework extensions
          _ → onErr []
    (_,_,errs) → onErr errs
