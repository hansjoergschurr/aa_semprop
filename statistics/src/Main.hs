import System.IO
import System.Environment
import System.Console.GetOpt
import System.Exit
import Data.List
import Control.Monad
import Control.Applicative
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as B
import Text.Printf(printf)
import Data.Maybe
import Text.Read(readMaybe)

import Shelly

import CallAnalyze

data Flag = FApxDir String | FFramework String | FSemantics (Maybe String) |
              FTimelimit (Maybe String) | FDeleteLogs | FProperties (Maybe String)
    deriving (Eq,Ord,Show)

flags = [Option ['a'] [] (ReqArg FApxDir "APX_DIR")
            "Directory containing the Aspartix ASP encodings.",
          Option ['f'] [] (ReqArg FFramework "FRAMEWORK_DIR")
            "Directory containing the the argumentation frameworks in APX format.",
          Option ['d'] [] (NoArg FDeleteLogs)
            "Delete the clingo logs after analyzing them.",
          Option ['t'] [] (OptArg FTimelimit "TIMELIMIT")
            "Timelimit given to clingo. The default is 120 seconds. A timelimit of 0 deactivates the timelimiting.",
          Option ['p'] [] (OptArg FProperties "PROPERTIES")
            "Properties to generate as one letter codes.",
          Option ['s'] [] (OptArg FSemantics "SEMANTICS")
            (unlines ["Comma seperated list of of the program files in the APX_DIR folder used to generate the extensions.",
            "Default: '" ++ T.unpack ( T.intercalate "," allSemantics) ++ "'."])
          ]

header = "Usage: statistics -a APX_DIR -f FRAMEWORK_DIR [-d] [-tTIMELIMIT] [-pPROPERTIES] [-sSEMANTICS] [OUTPUTFILE]\n"

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
getSemantics (FSemantics (Just s):_) = map T.strip (T.split (==',') $ T.pack s)
getSemantics (_:xs) = getSemantics xs

getTimelimit [] = 120
getTimelimit (FTimelimit (Just s):_) = fromMaybe 120 (readMaybe s)
getTimelimit (FTimelimit Nothing:_) = 120
getTimelimit (_:xs) = getTimelimit xs

getDeleteLogs [] = False
getDeleteLogs (FDeleteLogs:_) = True
getDeleteLogs (_:xs) = getDeleteLogs xs

-- Selectable properties support
decodeProp 'a' = "Arguments"
decodeProp 'e' = "Extensions"
decodeProp 'd' = "Downwards Closed"
decodeProp 't' = "Tight"
decodeProp 'c' = "Conflict Sensitive"
decodeProp 's' = "In Signatures Of"
decodeProp 'r' = "Rejected Arguments"
decodeProp 'i' = "Implicit Conflicts"
decodeProp 'j' = "Implicit Conflicts not Rejected"

defaultProp = "erij"

getProperties [] = (defaultProp, map decodeProp defaultProp)
getProperties (FProperties (Just s):_) = (s, map decodeProp s)
getProperties (FProperties Nothing:_) = (defaultProp, map decodeProp defaultProp)
getProperties (_:xs) = getProperties xs

onErr e = do
  hPutStrLn stderr (concat e ++ usageInfo header flags)
  exitWith (ExitFailure 1)

semDir dir sem = dir </> sem

findExtensions ∷ Integer → (T.Text → Shelly.FilePath) → Shelly.FilePath → T.Text → Sh (T.Text, Maybe T.Text)
findExtensions timelimit semDir frame sem = sub $ escaping False $ do
    dir ← cmd "dirname" [outfile]
    cmd "mkdir" ["-p", dir]
    run_ (fromText c) []
    code <- lastExitCode
    return (toTextIgnore frame, if code == 11 then Nothing else Just outfile)
  where
    up = T.unpack.toTextIgnore
    f = up frame
    s = up $ semDir sem
    outfile = T.append "out/" $ T.intercalate "_" [toTextIgnore frame, sem, "log"]
    c = T.pack $ printf "clingo 0 -t 4 --time-limit=%d -W no-atom-undefined %s %s >> %s" timelimit f s (T.unpack outfile)

main ∷ IO ()
main = do
  hSetBuffering stdout LineBuffering
  args ← getArgs
  case getOpt Permute flags args of
    (args,argv,[]) →
        case (getFrameworkPath args, getApxDirPath args) of
          (Just framework, Just extensions) → do
            let semantics = getSemantics args
            let timelimit = getTimelimit args
            let deleteExtensions = getDeleteLogs args
            let props = getProperties args
            shelly $ verbosely $ errExit False $ do
              pwd >>= appendToPath
              echo $ T.append "Framwork path: " framework
              echo $ T.append "Aspartix path: " extensions
              echo $ T.append "Using semantics: " $ T.intercalate ", " semantics

              let output = fromText . T.pack $ head (argv++["output.csv"])
              frames ← findWhen (return.hasExt "apx") $ fromText framework
              let f = findExtensions timelimit $ semDir extensions
              csvHeader props output semantics
              mapM_ (generateStatistics props deleteExtensions output semantics) $ f <$> frames
          _ → onErr []
    (_,_,errs) → onErr errs
