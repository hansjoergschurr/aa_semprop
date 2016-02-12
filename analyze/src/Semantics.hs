module Semantics(outputSemanticProperties) where

import Extensions
import Frameworks

import Data.Set hiding (map)
import qualified Data.List as L
import qualified Data.Map.Strict as M

import qualified Data.ByteString.Lazy.Char8 as B

rejectedArguments ∷ Framework → Extensions → Set Argument
rejectedArguments (Framework args _) (Extensions exts) =
  args \\ unions exts

unsortedPairs ∷ Ord a ⇒ Set a → Set (a,a)
unsortedPairs x = fromDistinctAscList [(x,y) | x←xs, y←xs, x < y]
  where
    xs = toAscList x

srt ∷ Ord a ⇒ (a,a) → (a,a)
srt t@(a,b)
  | a>b = (b,a)
  | otherwise = t

implicitConflicts ∷ Framework → Extensions → Set (Argument, Argument)
implicitConflicts (Framework args atcs) (Extensions exts) =
    allP \\ expl \\ inExt
  where
    allP = unsortedPairs args
    expl = fromList [srt (k,b) | (k,v) ← M.toList atcs, b ← v]
    inExt = unions $ map unsortedPairs exts

numArguments ∷ Framework → Extensions → Int
numArguments (Framework args _) _ = length args

numExtensions∷ Framework → Extensions → Int
numExtensions _ (Extensions e) = length e

-- output and pretty printing

class PrettyOut a where
  pretty ∷ a → String

-- this is all very inefficient
instance PrettyOut Int where
  pretty = show
instance PrettyOut B.ByteString where
  pretty = B.unpack
instance PrettyOut a ⇒ PrettyOut (Set a) where
  pretty s = concat.(L.intersperse ",") $ map pretty (toList s)
instance (PrettyOut a,PrettyOut b) ⇒ PrettyOut (a,b) where
  pretty (x,y) = "("++(pretty x)++","++(pretty y)++")"


outputSemanticProperties f e = do
  outputLine f e "Arguments" numArguments
  outputLine f e "Extensions" numExtensions
  outputLine f e "Rejected Arguments" rejectedArguments
  outputLine f e "Implicit Conflicts" implicitConflicts

outputLine ∷ PrettyOut a => Framework → Extensions → String → (Framework → Extensions → a) → IO  ()
outputLine f e n s = putStrLn $ n ++ "\t" ++ pretty (s f e)
