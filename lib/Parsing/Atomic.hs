
module Parsing.Atomic where

import           Text.Megaparsec
import           Text.Megaparsec.String

import           Parsing
import           Tactic

admitP :: Parser Atomic
admitP = try (rword "admit") >> return Admit

exactP :: Parser Atomic
exactP = Exact <$> (try (rword "exact") >> variableP)

introP :: Parser Atomic
introP = Intro <$> (try (rword "intro") >> binderP)
