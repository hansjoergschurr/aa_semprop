module Semantics (Framework) where

import Data.Set
import qualified Data.Map.Strict as M

import qualified Data.ByteString.Lazy.Char8 as B
import Text.Regex.Posix

type Argument = B.ByteString

-- A framework consists of a set of arguments and a map from arguments to
-- arguments attacket by an argument.
data Framework = Framework (Set Argument) (M.Map Argument [Argument])

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
        where ns = insert arg args
      _ → case att of
        [[_,arg1,arg2]] → Framework args (M.insertWith (++) arg1 [arg2] attacks)
        _ → f
