module Semantics(outputSemanticProperties) where

import Extensions
import Frameworks

import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map.Strict as M

import qualified Data.ByteString.Lazy.Char8 as B

unionAll ∷ (Ord a) ⇒ S.Set (S.Set a) → S.Set a
unionAll = S.fold S.union S.empty

rejectedArguments ∷ Framework → Extensions → S.Set Argument
rejectedArguments (Framework args _) (Extensions exts) =
  args S.\\ unionAll exts

unsortedPairs ∷ Ord a ⇒ S.Set a → S.Set (a,a)
unsortedPairs x = S.fromDistinctAscList [(x,y) | x←xs, y←xs, x < y]
  where
    xs = S.toAscList x

slift ∷ (Ord a, Ord b, Ord c) ⇒ (a→b→c) → S.Set a → S.Set b → S.Set c
slift f s0 s1 = unionAll (S.map (\e → S.map (f e) s1) s0)

forAll ∷ (Ord a) ⇒ (a → Bool) → S.Set a → Bool
forAll f = S.fold (\x b→b && f x) True

exists ∷ (Ord a) ⇒ (a → Bool) → S.Set a → Bool
exists f = S.fold (\x b→b || f x) False

-- {{}, {a}}
makeSet :: (Ord a) ⇒ a → S.Set (S.Set a)
makeSet = S.insert S.empty . S.singleton.S.singleton

powerSet ∷ (Ord a) ⇒ S.Set a → S.Set (S.Set a)
powerSet = S.fold (slift S.union) (S.singleton S.empty) . S.map makeSet

srt ∷ Ord a ⇒ a → a → (a,a)
srt a b
  | a>b = (b,a)
  | otherwise = (a,b)

srtt ∷ Ord a ⇒ (a,a) → (a,a)
srtt = uncurry srt

downwardsClosed ∷ Framework → Extensions → Bool
downwardsClosed _ (Extensions e) =  e /= closure
  where
    closure = unionAll $ S.map powerSet e

tight ∷ Framework → Extensions → Bool
tight _ (Extensions e) = forAll noConflict e
  where
    pairs = unionAll $ S.map unsortedPairs e
    isConflict a = any (\s→S.notMember (srt s a) pairs)
    checkConflict s a = S.insert a s  `S.member` e || isConflict a s
    args = unionAll e
    noConflict s = forAll (checkConflict s) args

implicitConflicts ∷ Framework → Extensions → S.Set (Argument, Argument)
implicitConflicts (Framework args atcs) (Extensions exts) =
    allP S.\\ expl S.\\ inExt
  where
    allP = unsortedPairs args
    expl = S.fromList [srt k b | (k,v) ← M.toList atcs, b ← v]
    inExt = unionAll $ S.map unsortedPairs exts

conflictSensitive ∷ Framework → Extensions → Bool
conflictSensitive _ (Extensions e) = forAll (\a→forAll (cond a) e) e
    where
      pairs = unionAll $ S.map unsortedPairs e
      cond a b = (c `S.member` e) || conf c
        where c = a `S.union` b
      conf c = exists (\a→exists (\b→srt a b `S.notMember` pairs) c) c


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
instance PrettyOut Bool where
  pretty = show
instance PrettyOut B.ByteString where
  pretty = B.unpack
instance PrettyOut a ⇒ PrettyOut (S.Set a) where
  pretty s = L.intercalate "," $ map pretty (S.toList s)
instance (PrettyOut a,PrettyOut b) ⇒ PrettyOut (a,b) where
  pretty (x,y) = "("++pretty x++","++pretty y++")"


outputSemanticProperties f e = do
  outputLine f e "Arguments" numArguments
  outputLine f e "Extensions" numExtensions
  outputLine f e "Rejected Arguments" rejectedArguments
  outputLine f e "Implicit Conflicts" implicitConflicts
  outputLine f e "Implicit Conflicts" implicitConflicts
  outputLine f e "Downwards Closed" downwardsClosed
  outputLine f e "Tight" tight
  outputLine f e "Conflict Sensitive" conflictSensitive

outputLine ∷ PrettyOut a => Framework → Extensions → String → (Framework → Extensions → a) → IO  ()
outputLine f e n s = putStrLn $ n ++ "\t" ++ pretty (s f e)
