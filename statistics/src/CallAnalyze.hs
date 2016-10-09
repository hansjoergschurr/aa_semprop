module CallAnalyze (generateStatistics, csvHeader) where

import Control.Applicative
import Control.Monad
import Data.Maybe
import qualified Data.Text as T
import Shelly

analyze =
  command "analyze" ["-n"]

analyzeFramework includeArguments (f,e) = sub $ silently $ do
  echo $ T.concat ["Analyzing: ", f, ", ", e]
  out ← analyze ["-f", f, "-e", e]
  let dict = map (T.break (== '\t')) $ T.lines out
  let props = ["Extensions", "Rejected Arguments", "Implicit Conflicts", "Implicit Conflicts not Rejected"]
  let allProps = if includeArguments then "Arguments":props else props
  return $ map (T.stripStart.fromJust.(`lookup` dict)) allProps

-- Writes the statistics for one framework
generateStatistics ∷ Shelly.FilePath → [T.Text] → (T.Text → Sh (T.Text, T.Text))→ Sh ()
generateStatistics outfile semantics extF = do
  (l:logs) ← sequence $ extF <$> semantics
  stats ← sequence $ analyzeFramework True l:map (analyzeFramework False) logs
  let frame = fst $ head logs
  appendfile outfile $ T.intercalate "," (frame:concat stats)
  appendfile outfile "\n"

-- Generate the csv header
csvHeader ∷ Shelly.FilePath → [T.Text] → Sh ()
csvHeader outfile semantics = do
  let one = "frame"
  let two = "args"
  let three = [T.append s pf | s <- semantics, pf <- ["_ext", "_rj", "_imp", "_impnrj"]]
  appendfile outfile $ T.intercalate "," (one:two:three)
  appendfile outfile "\n"
