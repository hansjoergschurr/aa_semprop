module Semantics (Extrensions) where

import Data.Set

data Extensions a = Extensions {
  arguments ∷ Data.Set a,
  extensions ∷ [Data.Set a] }



parseAPX ∷ String → Extensions
