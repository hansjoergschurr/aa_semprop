module Extensions(Extensions, readClasp, readIccma) where

import qualified Data.Set as S
import qualified Data.ByteString.Lazy.Char8 as B
import Text.Regex.Posix

import Frameworks (Argument)

data Extensions = Extensions [S.Set Argument]
  deriving Show

-- The ICCMA format describes extensions as list of lists:
-- [[A1,A2,...,AN],[B1,B2,...,BM],..., [Z1,Z2,...,ZN]]

iccmaExtension = B.pack "\\[([A-Za-z0-9,]+)\\]" -- captures one extension

readIccma ∷ [B.ByteString] → Extensions
readIccma (l:_) = let
      extStrs = map (!! 1) (l =~ iccmaExtension ∷ [[B.ByteString]])
    in
      Extensions $ map (S.fromList.B.split ',') extStrs

-- clasp log.
claspAnswerStart = B.pack "^Answer:" -- line just before an answer line
claspInExtension = B.pack "in\\([ \\t]*([a-z0-9]+)[ \\t]*\\)" -- an answer line

readClasp' ∷ [B.ByteString] → [S.Set Argument]
readClasp' (l1:l2:ls)
  | l1 =~ claspAnswerStart ∷ Bool = let
      ins = map (!! 1) (l2 =~ claspInExtension :: [[B.ByteString]])
    in S.fromList ins:readClasp' ls
  | otherwise = readClasp' (l2:ls)
readClasp' _ = []

readClasp ∷ [B.ByteString] → Extensions
readClasp l = Extensions $ readClasp' l
