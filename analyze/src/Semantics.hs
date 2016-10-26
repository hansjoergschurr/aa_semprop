module Semantics(outputSemanticProperties, isDownwardsClosed, sanityCheck) where

import Extensions
import Frameworks

import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Control.Applicative (liftA2)
import Debug.Trace

import qualified Data.ByteString.Lazy.Char8 as B

unionAll ∷ (Ord a) ⇒ S.Set (S.Set a) → S.Set a
unionAll = S.fold S.union S.empty

collectRejectedArgs ∷ Framework → Extensions → S.Set Argument
collectRejectedArgs (Framework args _) (Extensions exts) =
  args S.\\ unionAll exts

unsortedPairs ∷ Ord a ⇒ S.Set a → S.Set (a,a)
unsortedPairs x = S.fromDistinctAscList [(x,y) | x←xs, y←xs, x < y]
  where
    xs = S.toAscList x

srt ∷ Ord a ⇒ a → a → (a,a)
srt a b
  | a>b = (b,a)
  | otherwise = (a,b)

subsetOfAny :: Ord a => S.Set a -> S.Set (S.Set a) -> Bool
subsetOfAny b = any (S.isSubsetOf b)

-- The subset-maximal sets.
bases :: Ord a => S.Set (S.Set a) -> S.Set (S.Set a)
bases = S.foldl' (\b s-> if s `subsetOfAny` b then b else S.insert s b) S.empty

isDownwardsClosed ∷ Extensions → Bool
isDownwardsClosed (Extensions e) = searchSubsets (u, S.empty)
  where
    b = bases e
    u = S.toAscList $ unionAll b
    -- Searches the subsets, starting with the unit sets.
    -- If a subset is not a subset of any base, its supersets are skipped.
    searchSubsets ([], set) = set `S.member` e
    searchSubsets (res, set)
      | not $ set `S.member` e = False
      | otherwise = all searchSubsets $ gen res set
    gen [] _ = []
    gen (x:xs) set
      | ns `subsetOfAny` b = (xs, ns):gen xs set
      | otherwise = gen xs set
        where
          ns = S.insert x set

isTight ∷ Extensions → Bool
isTight = tightness S.member

tightness ∷ (S.Set Argument -> S.Set (S.Set Argument) -> Bool) -> Extensions → Bool
tightness test (Extensions e) = all noConflict e
  where
    args = unionAll e
    noConflict s = all (checkConflict s) args
    checkConflict s a = S.insert a s `test` e || isConflict a s
    isConflict a = any (\s→S.notMember (srt s a) pairs)
    pairs = unionAll $ S.map unsortedPairs e

collectImplicitConflicts ∷ Framework → Extensions → S.Set (Argument, Argument)
collectImplicitConflicts (Framework args atcs) (Extensions exts) =
    allP S.\\ expl S.\\ inExt
  where
    allP = unsortedPairs args
    expl = S.fromList [srt k b | (k,v) ← M.toList atcs, b ← v]
    inExt = unionAll $ S.map unsortedPairs exts

isIncomparable ∷ Extensions → Bool
isIncomparable (Extensions e) = all compA e
  where
    compA a = all (comp a) e
    comp a b = not (a `S.isSubsetOf` b || b `S.isSubsetOf` a)

isConflictSensitive ∷ Extensions → Bool
isConflictSensitive (Extensions e) = all (\a→all (cond a) e) e
    where
      pairs = unionAll $ S.map unsortedPairs e
      cond a b = (c `S.member` e) || conf c
        where c = a `S.union` b
      conf c = any (\a→any (\b→srt a b `S.notMember` pairs) c) c

numArguments ∷ Framework → Int
numArguments (Framework args _) = length args

numExtensions∷ Extensions → Int
numExtensions (Extensions e) = length e

data SemanticProperties = SemanticProperties {
  arguments ∷ Int,
  extensions ∷ Int,
  rejectedArguments ∷ S.Set Argument,
  implicitConflicts ∷ S.Set (Argument, Argument),
  hasExtensions ∷ Bool,
  emptyExtension ∷ Bool,
  incomparable ∷ Bool,
  downwardsClosed ∷ Bool,
  tight ∷ Bool,
  closureIsTight ∷ Bool,
  conflictSensitive ∷ Bool}

inSignatures ∷ SemanticProperties → StringList
inSignatures sp = StringList $ mapMaybe c
    [(cf,"cof"),(naive,"nai"),(stb,"stb"),(stage,"stg"),(adm,"adm"),(pref,"prf"),(sem,"sem")]
  where
    (<&>)  = liftA2 (&&)
    c (f, s) = if f sp then Just s else Nothing
    cf = hasExtensions <&> downwardsClosed <&> tight
    naive = hasExtensions <&> incomparable <&> closureIsTight
    stb = incomparable <&> tight
    stage = hasExtensions <&> stb
    adm = hasExtensions <&> conflictSensitive <&> emptyExtension
    pref = hasExtensions <&> incomparable <&> conflictSensitive
    sem = pref

-- output and pretty printing
class PrettyOut a where
  pretty ∷ a → String

-- this is all very inefficient
newtype StringList = StringList [String]

instance PrettyOut Int where
  pretty = show
instance PrettyOut StringList where
  pretty (StringList s) = L.intercalate "," s
instance PrettyOut Bool where
  pretty = show
instance PrettyOut B.ByteString where
  pretty = B.unpack
instance PrettyOut a ⇒ PrettyOut (S.Set a) where
  pretty s = L.intercalate "," $ map pretty (S.toList s)
instance (PrettyOut a,PrettyOut b) ⇒ PrettyOut (a,b) where
  pretty (x,y) = "("++pretty x++","++pretty y++")"

semanticProperties f e = SemanticProperties {
  arguments = numArguments f,
  extensions = numExtensions e,
  rejectedArguments = collectRejectedArgs f e,
  implicitConflicts = collectImplicitConflicts f e,
  hasExtensions = (\(Extensions s)→ not $ S.null s) e,
  emptyExtension = (\(Extensions s)→ S.member S.empty s) e,
  incomparable = isIncomparable e,
  downwardsClosed = isDownwardsClosed e,
  tight = isTight e,
  closureIsTight = tightness subsetOfAny e,
    --isTight $ (\(Extensions s) → Extensions (closure s)) e,
  conflictSensitive = isConflictSensitive e}

filterConflictByArguments ∷  S.Set Argument → S.Set (Argument, Argument) → S.Set (Argument, Argument)
filterConflictByArguments c = S.filter f
  where f (a,b) = not (a `S.member` c || b `S.member` c)

outLists False sp = do
  outputLine "Rejected Arguments" $ rejectedArguments sp
  outputLine "Implicit Conflicts" $ implicitConflicts sp
  outputLine "Implicit Conflicts not Rejected" $
    filterConflictByArguments (rejectedArguments sp) (implicitConflicts sp)
outLists True sp = do
  outputLine "Rejected Arguments" $ S.size (rejectedArguments sp)
  outputLine "Implicit Conflicts" $ S.size (implicitConflicts sp)
  outputLine "Implicit Conflicts not Rejected" $
    S.size (filterConflictByArguments (rejectedArguments sp) (implicitConflicts sp))

outputSemanticProperties n f e = do
  let sp = semanticProperties f e
  outputLine "Arguments" $ arguments sp
  outputLine "Extensions" $ extensions sp
  outputLine "Downwards Closed" $ downwardsClosed sp
  outputLine "Tight" $ tight sp
  outputLine "Conflict Sensitive" $ conflictSensitive sp
  outputLine "In Signatures Of" $ inSignatures sp
  outLists n sp

outputLine ∷ PrettyOut a => String → a → IO  ()
outputLine m n = putStrLn $ m ++ "\t" ++ pretty n

sanityCheck ∷ Framework → Extensions → Maybe String
sanityCheck (Framework a atk) (Extensions e)
  | S.null a = Just "Empty framework."
  | not (unionAll e `S.isSubsetOf` a) = Just "Additional arguments in extensions."
  | otherwise = Nothing
