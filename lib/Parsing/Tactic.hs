
module Parsing.Tactic where

import           Text.Megaparsec.Combinator
import           Text.Megaparsec.String

import           Parsing
import           Parsing.Atomic
import           Parsing.Utils
import           Tactic

atomicP :: Parser Tactic
atomicP =
  Atomic <$> choice
  [ admitP
  , exactP
  , introP
  ]

semicolonP :: Parser Tactic
semicolonP = chainl1 atomicP (symbol ";" *> return Semicolon)

tacticP :: Parser Tactic
tacticP =
  choice
  [ semicolonP
  , atomicP
  ]
