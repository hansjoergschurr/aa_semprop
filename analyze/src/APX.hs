module APX (parse) where

import Semantic(Extensions)
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Languageimport Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

parse ∷ String → Extensions


apxParser = do
  eof
  return expression


-- have: identifier, arg(i)., att(i,j).
