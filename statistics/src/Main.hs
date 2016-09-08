import System.IO
import System.Environment
import System.Console.GetOpt
import System.Exit
import Data.List
import Shelly
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as B

data Flag = FApxDir String | FFramework String | FSemantics (Maybe String)
    deriving (Eq,Ord,Show)

flags = [Option ['a'] [] (ReqArg FApxDir "APX_DIR")
            "Directory containing the Aspartix ASP encodings.",
          Option ['f'] [] (ReqArg FFramework "FRAMEWORK_DIR")
            "Directory containing the the argumentation frameworks in APX format.",
          Option ['s'] [] (OptArg FSemantics "SEMANTICS")
            "Comma seprerated list of three letter names of the semantics to calculate."
          ]

header = "Usage: semantics -a APX_DIR -f FRAMEWORK_DIR [-sSEMANTICS]\n"

getFrameworkPath [] = Nothing
getFrameworkPath (FFramework s:_) = Just $ T.pack s
getFrameworkPath (_:xs) = getFrameworkPath xs

getApxDirPath [] = Nothing
getApxDirPath (FApxDir s:_) = Just $ T.pack s
getApxDirPath (_:xs) = getApxDirPath xs

allSemantics = ["adm", "stb", "prf", "stg", "sem"] -- "nai",

getSemantics [] = allSemantics
getSemantics (FSemantics (Just []):_) = allSemantics
getSemantics (FSemantics Nothing:_) = allSemantics
getSemantics (FSemantics (Just s):_) = allSemantics `intersect` map T.strip (T.split (==',') $ T.pack s)
getSemantics (_:xs) = getSemantics xs

onErr e = do
  hPutStrLn stderr (concat e ++ usageInfo header flags)
  exitWith (ExitFailure 1)

findExtensions f_dir sem_dir frame sem = sub $ escaping False $ run_ c []
  where
    c = T.intercalcate " " ["clingo", f, s


main ∷ IO ()
main = do
  hSetBuffering stdout LineBuffering
  args ← getArgs
  case getOpt Permute flags args of
    (args,_,[]) →
        case (getFrameworkPath args, getApxDirPath args) of
          (Just framework, Just extensions) → do
            let semantics = getSemantics args
            shelly $ verbosely $ do
              echo $ T.append "Framwork path: " framework
              echo $ T.append "Aspartix path: " framework
              echo $ T.append "Using semantics: " $ T.intercalate ", " semantics
          _ → onErr []
    (_,_,errs) → onErr errs
