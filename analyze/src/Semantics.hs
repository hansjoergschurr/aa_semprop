module Semantics(outputSemanticProperties, sanityCheck) where

import Extensions
import Frameworks

import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Control.Applicative (liftA2)

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

closure = unionAll . S.map powerSet

isDownwardsClosed ∷ Extensions → Bool
isDownwardsClosed (Extensions e) =  e /= closure e

isTight ∷ Extensions → Bool
isTight (Extensions e) = forAll noConflict e
  where
    pairs = unionAll $ S.map unsortedPairs e
    isConflict a = any (\s→S.notMember (srt s a) pairs)
    checkConflict s a = S.insert a s  `S.member` e || isConflict a s
    args = unionAll e
    noConflict s = forAll (checkConflict s) args

collectImplicitConflicts ∷ Framework → Extensions → S.Set (Argument, Argument)
collectImplicitConflicts (Framework args atcs) (Extensions exts) =
    allP S.\\ expl S.\\ inExt
  where
    allP = unsortedPairs args
    expl = S.fromList [srt k b | (k,v) ← M.toList atcs, b ← v]
    inExt = unionAll $ S.map unsortedPairs exts

isIncomparable ∷ Extensions → Bool
isIncomparable (Extensions e) = forAll compA e
  where
    compA a = forAll (comp a) e
    comp a b = not (a `S.isSubsetOf` b || b `S.isSubsetOf` a)

isConflictSensitive ∷ Extensions → Bool
isConflictSensitive (Extensions e) = forAll (\a→forAll (cond a) e) e
    where
      pairs = unionAll $ S.map unsortedPairs e
      cond a b = (c `S.member` e) || conf c
        where c = a `S.union` b
      conf c = exists (\a→exists (\b→srt a b `S.notMember` pairs) c) c

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
  closureIsTight = isTight $ (\(Extensions s) → Extensions (closure s)) e,
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
