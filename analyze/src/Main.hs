import qualified Data.ByteString.Lazy.Char8 as B
import System.IO
import System.Environment
import System.Console.GetOpt
import System.Exit

import Frameworks
import Extensions
import Semantics

data Flag = FTgf | FIccma | FFramework String | FExtensions String
    deriving (Eq,Ord,Show)

flags = [Option ['t'] [] (NoArg FTgf)
            "The framework is in Trivial Graph Format, instead of Aspartix Format.",
         Option ['c'] [] (NoArg FIccma)
            (unlines ["The exstensions are in ICCMA competiton format, instead of an output log of clasp.",
             "a theorem iff the QBF formular is false."]),
         Option ['f'] [] (ReqArg FFramework "FILE")
            "FILE containing the framework.",
         Option ['e'] [] (ReqArg FExtensions "FILE")
            "FILE containing the extensions."]

header = "Usage: analyze [-t] [-c] -f FILE -e FILE\n"

getFrameworkPath [] = Nothing
getFrameworkPath (FFramework s:_) = Just s
getFrameworkPath (_:xs) = getFrameworkPath xs

getExstensionsPath [] = Nothing
getExstensionsPath (FExtensions s:_) = Just s
getExstensionsPath (_:xs) = getExstensionsPath xs

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
        case (getFrameworkPath args, getExstensionsPath args) of
          (Just framework, Just extensions) → do
            let fParser = if FTgf `elem` args then readTgf else readApx
            let eParser = if FIccma `elem` args then readIccma else readClasp

            framework ← readFromFile framework fParser
            extensions ← readFromFile extensions eParser

            putStrLn $ "Arguments\t" ++ show (numArguments framework extensions)
            putStrLn $ "Extensions\t" ++ show (numExtensions framework extensions)
            putStrLn $ "Rejected Arguments\t" ++ show (rejectedArguments framework extensions)
            putStrLn $ "Implicit Conflicts\t" ++ show (implicitConflicts framework extensions)

            print framework
            print extensions
          _ → onErr []
    (_,_,errs) → onErr errs
