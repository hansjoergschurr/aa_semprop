import qualified Data.ByteString.Lazy.Char8 as B
import System.IO
import System.Environment
import System.Console.GetOpt
import System.Exit

import Frameworks
import Extensions

data Flag = Tgf | Iccma | Framework String | Extensions String
    deriving (Eq,Ord,Show)

flags = [Option ['t'] [] (NoArg Tgf)
            "The framework is in Trivial Graph Format, instead of Aspartix Format.",
         Option ['c'] [] (NoArg Iccma)
            (unlines ["The exstensions are in ICCMA competiton format, instead of an output log of clasp.",
             "a theorem iff the QBF formular is false."]),
         Option ['f'] [] (ReqArg Framework "FILE")
            "FILE containing the framework.",
         Option ['e'] [] (ReqArg Extensions "FILE")
            "FILE containing the extensions."]

header = "Usage: analyze [-t] [-c] -f FILE -e FILE\n"

getFrameworkPath [] = Nothing
getFrameworkPath (Framework s:_) = Just s
getFrameworkPath (_:xs) = getFrameworkPath xs

getExstensionsPath [] = Nothing
getExstensionsPath (Extensions s:_) = Just s
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
            let fParser = if Tgf `elem` args then readTgf else readApx
            let eParser = if Iccma `elem` args then readIccma else readClasp

            framework ← readFromFile framework fParser
            extensions ← readFromFile extensions eParser

            print framework
            print extensions
          _ → onErr []
    (_,_,errs) → onErr errs
