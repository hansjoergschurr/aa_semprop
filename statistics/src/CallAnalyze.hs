module CallAnalyze (generateStatistics, csvHeader) where

import Control.Applicative
import Control.Monad
import Data.Maybe
import qualified Data.Text as T
import Shelly

analyze =
  command "analyze" ["-n"]

emptRes s = map (const s)

analyzeFramework _ props (nargs, os) (_, Nothing) =  return (nargs, emptRes "NaN" props:os)
analyzeFramework dl props (nargs, os) (f,Just e) = sub $ silently $ do
  echo $ T.concat ["Analyzing: ", f, ", ", e]
  out ← analyze ["-f", f, "-e", e]
  code ← lastExitCode
  when dl $ run_ "rm" ["-f", e]
  if code == 0 then do
    let dict = map (T.break (== '\t')) $ T.lines out
    let nargs' = "Arguments" `lookup` dict
    return (nargs <|> nargs', map (T.stripStart.fromJust.(`lookup` dict)) props:os)
  else
    return (nargs, emptRes "Err" props:os)

-- Writes the statistics for one framework
generateStatistics ∷ [T.Text] → Bool → Shelly.FilePath → [T.Text] → (T.Text → Sh (T.Text, Maybe T.Text))→ Sh ()
generateStatistics requestedProps deleteExtensions outfile semantics extF = do
  logs ← sequence $ extF <$> semantics
  let frame = fst $ head logs
  anaRes ← foldM (analyzeFramework deleteExtensions requestedProps) (Nothing, []) logs
  case anaRes of
    (Just na, stats) → do
      appendfile outfile $ T.intercalate "," (frame:na:(concat.reverse) stats)
      appendfile outfile "\n"
    (Nothing, _) →
      echo $ T.append "Skiped: " frame

-- Generate the csv header
csvHeader ∷ [T.Text] → Shelly.FilePath → [T.Text] → Sh ()
csvHeader requestedProps outfile semantics = do
  let one = "frame"
  let two = "args"
  let three = [T.concat [s," ",pf] | s ← semantics, pf ← requestedProps]
  appendfile outfile $ T.intercalate "," (one:two:three)
  appendfile outfile "\n"
