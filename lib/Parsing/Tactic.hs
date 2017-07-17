
module Parsing.Tactic
  ( atomicP
  , tacticP
  ) where

import Text.Megaparsec.Combinator
import Text.Megaparsec.String

import Parsing
import Parsing.Atomic
import Parsing.Utils
import Tactic
import Term.Variable

atomicP :: Parser (Tactic Variable)
atomicP =
  Atomic <$> choice
  [ admitP
  , exactP
  , introP
  ]

semicolonP :: Parser (Tactic Variable)
semicolonP = chainl1 atomicP (symbol ";" *> return Semicolon)

tacticP :: Parser (Tactic Variable)
tacticP =
  choice
  [ semicolonP
  , atomicP
  ]
