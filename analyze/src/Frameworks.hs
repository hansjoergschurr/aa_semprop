module Semantics (Extrensions) where

import Data.Set
import qualified Data.Map.Strict as M

import qualified Data.ByteString.Lazy as B
import Text.Regex.Posix

type Argument = B.ByteString

-- A framework consists of a set of arguments and a map from arguments to
-- arguments attacket by an argument.
data Framework = Framework (Data.Set Argument) (Data.Map Argument [Argument])

apxArg = L.pack "^arg\\([ \\t]*([a-z0-9]+)[ \\t]*\\)\\.$"
apxAtt = L.pack "^att\\([ \\t]*([a-z0-9]+)[ \\t]*,[ \\t]*([a-z0-9]+)[ \\t]*\\)\\.$"

apxParseLine ∷ Framework → B.ByteString  → Framework
apxParseLine f@(Framework args attacks) str =
  let
    arg = str =~ apxArg
    att = str =~ apxAtt
  in
    case arg of
      [[_,arg]] → Framework (insert arg args) attacks
      _ → case att of
        [[_,arg1,arg2]] → Framework args (M.insertWith (++) arg1 [arg2] attacks)
        _ → f
