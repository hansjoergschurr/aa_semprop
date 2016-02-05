module Frameworks (Framework, readApx, readTgf) where

import qualified Data.Set as S
import qualified Data.Map.Strict as M

import qualified Data.ByteString.Lazy.Char8 as B
import Text.Regex.Posix

type Argument = B.ByteString

-- A framework consists of a set of arguments and a map from arguments to
-- arguments attacket by an argument.
data Framework = Framework (S.Set Argument) (M.Map Argument [Argument])
  deriving Show

apxArg = B.pack "^arg\\([ \\t]*([a-z0-9]+)[ \\t]*\\)\\.$"
apxAtt = B.pack "^att\\([ \\t]*([a-z0-9]+)[ \\t]*,[ \\t]*([a-z0-9]+)[ \\t]*\\)\\.$"

apxParseLine ∷ Framework → B.ByteString  → Framework
apxParseLine f@(Framework args attacks) str =
  let
    arg = str =~ apxArg ∷ [[B.ByteString]]
    att = str =~ apxAtt ∷ [[B.ByteString]]
  in
    case arg of
      [[_,arg]] → Framework ns attacks
        where ns = S.insert arg args
      _ → case att of
        [[_,arg1,arg2]] → Framework args (M.insertWith (++) arg1 [arg2] attacks)
        _ → f

readApx ∷ [B.ByteString] → Framework
readApx = foldl apxParseLine (Framework S.empty M.empty)

tgfArg = B.pack "^[ \\t]*([0-9]+).*$"
tgfAtt = B.pack "^[ \\t]*([0-9]+)[ \\t]+([0-9]+).*$"
tgfSep = B.pack "#"
data  TgfState = Arguments | Attacks

tgfParseLine ∷  (TgfState, Framework) → B.ByteString → (TgfState, Framework)
tgfParseLine (Arguments, f@(Framework args attacks)) str
  | str == tgfSep = (Attacks, f)
  | otherwise =
    let
      [[_,arg]] = str =~ tgfArg ∷ [[B.ByteString]]
      nargs = S.insert arg args
    in
      (Arguments, Framework nargs attacks)
tgfParseLine (Attacks, f@(Framework args attacks)) str =
  let
    [[_,arg1, arg2]] = str =~ tgfAtt ∷ [[B.ByteString]]
    natt = M.insertWith (++) arg1 [arg2] attacks
  in
    (Attacks, Framework args natt)

readTgf ∷ [B.ByteString] → Framework
readTgf = snd . foldl tgfParseLine (Arguments, Framework S.empty M.empty)
