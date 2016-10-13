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
  code <- lastExitCode
  if code == 0 then do
    let dict = map (T.break (== '\t')) $ T.lines out
    let props = ["Extensions", "Rejected Arguments", "Implicit Conflicts", "Implicit Conflicts not Rejected"]
    let allProps = if includeArguments then "Arguments":props else props
    return $ map (T.stripStart.fromJust.(`lookup` dict)) allProps
  else if includeArguments then
      return ["Err","Err", "Err","Err", "Err"]
    else
      return ["Err", "Err","Err", "Err"]

nan :: Sh [T.Text]
nan = return ["NaN", "NaN", "NaN", "NaN"]

ana [] = Nothing
ana ((_,Nothing):xs) = liftM2 (:) (Just nan) (ana xs)
ana ((f,Just e):xs) = Just $ analyzeFramework True (f,e):ana' xs

ana' [] = []
ana' ((f, Just e):xs) = analyzeFramework False (f,e):ana' xs
ana' ((_,Nothing):xs) = nan:ana' xs

-- Writes the statistics for one framework
generateStatistics ∷ Shelly.FilePath → [T.Text] → (T.Text → Sh (T.Text, Maybe T.Text))→ Sh ()
generateStatistics outfile semantics extF = do
  logs ← sequence $ extF <$> semantics
  let frame = fst $ head logs
  case ana logs of
    Just a -> do
      stats ← sequence a
      appendfile outfile $ T.intercalate "," (frame:concat stats)
      appendfile outfile "\n"
    Nothing ->
      echo $ T.append "Skiped: " frame

-- Generate the csv header
csvHeader ∷ Shelly.FilePath → [T.Text] → Sh ()
csvHeader outfile semantics = do
  let one = "frame"
  let two = "args"
  let three = [T.append s pf | s <- semantics, pf <- ["_ext", "_rj", "_imp", "_impnrj"]]
  appendfile outfile $ T.intercalate "," (one:two:three)
  appendfile outfile "\n"
