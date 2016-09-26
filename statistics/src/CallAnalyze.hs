module CallAnalyze (generateStatistics) where

import Control.Applicative
import Control.Monad
import Data.Maybe
import qualified Data.Text as T
import Shelly

analyze =
  command "analyze" ["-n"]

analyzeFramework (f,e) = sub $ silently $ do
  echo $ T.concat ["Analyzing: ", f, ", ", e]
  out ← analyze ["-f", f, "-e", e]
  let dict = map (T.break (== '\t')) $ T.lines out
  return $ map (T.stripStart.fromJust.(`lookup` dict)) ["Rejected Arguments", "Implicit Conflicts"]

-- Writes the statistics for one framework
generateStatistics ∷ Shelly.FilePath → [T.Text] → (T.Text → Sh (T.Text, T.Text))→ Sh ()
generateStatistics outfile semantics extF = do
  logs ← sequence $ extF <$> semantics
  stats ← mapM analyzeFramework logs
  let frame = fst $ head logs
  appendfile outfile $ T.intercalate "," (frame:concat stats)
  appendfile outfile "\n"
