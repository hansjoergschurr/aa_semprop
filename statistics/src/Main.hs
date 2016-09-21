import System.IO
import System.Environment
import System.Console.GetOpt
import System.Exit
import Data.List
import Control.Monad
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as B
import Text.Printf(printf)

import Shelly

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

semDir dir sem = dir </> s sem
  where
    s :: T.Text -> T.Text
    s "adm" = "adm.dl"
    s "stb" = "stable.dl"
    s "prf" = "prefex_gringo.lp"
    s "stg" = "stage_gringo.lp"
    s "sem" = "semi_stable_gringo.lp"

findExtensions :: (T.Text -> Shelly.FilePath) -> T.Text -> Shelly.FilePath -> Sh T.Text
findExtensions semDir sem frame = sub $ escaping False $ do
    dir <- cmd "dirname" [outfile]
    cmd "mkdir" ["-p", dir]
    run_ (fromText c) []
    return outfile
  where
    up = T.unpack.toTextIgnore
    f = up frame
    s = up $ semDir sem
    outfile = T.append "out/" $ T.intercalate "_" [toTextIgnore frame, sem, "log"]
    c = T.pack $ printf "clingo  0 %s %s >> %s" f s (T.unpack outfile)

generateStatistics :: Sh T.Text -> Sh ()
generateStatistics ext = do
  e <- ext
  echo e

main ∷ IO ()
main = do
  hSetBuffering stdout LineBuffering
  args ← getArgs
  case getOpt Permute flags args of
    (args,_,[]) →
        case (getFrameworkPath args, getApxDirPath args) of
          (Just framework, Just extensions) → do
            let semantics = getSemantics args
            shelly $ verbosely $ errExit False $ do
              echo $ T.append "Framwork path: " framework
              echo $ T.append "Aspartix path: " extensions
              echo $ T.append "Using semantics: " $ T.intercalate ", " semantics

              frames <- findWhen (return.hasExt "apx") $ fromText framework
              let exts = [findExtensions (semDir extensions) s f | s <- semantics, f <- frames]
              mapM_ generateStatistics exts
          _ → onErr []
    (_,_,errs) → onErr errs
