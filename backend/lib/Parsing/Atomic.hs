-- module Parsing.Atomic
--   ( admitP,
--     exactP,
--     introP,
--   )
-- where

-- import Atomic
-- import Parsing
-- import Term.Variable
-- import Text.Megaparsec
-- import Text.Megaparsec.String

-- admitP :: Parser (Atomic Variable)
-- admitP = try (rword "admit") >> return Admit

-- exactP :: Parser (Atomic Variable)
-- exactP = Exact <$> (try (rword "exact") >> variableP)

-- introP :: Parser (Atomic Variable)
-- introP = Intro <$> (try (rword "intro") >> binderP)
