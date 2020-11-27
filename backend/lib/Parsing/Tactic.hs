module Parsing.Tactic
-- ( atomicP,
-- tacticP,
  (
  )
where

-- import Parsing
-- import Parsing.Atomic
-- import Parsing.Utils
-- import Tactic
-- import Term.Variable
-- import Text.Megaparsec.Combinator
-- import Text.Megaparsec.String

-- atomicP :: Parser (Tactic Variable)
-- atomicP =
--   Atomic
--     <$> choice
--       [ admitP,
--         exactP,
--         introP
--       ]

-- semicolonP :: Parser (Tactic Variable)
-- semicolonP = chainl1 atomicP (symbol ";" $> Semicolon)

-- tacticP :: Parser (Tactic Variable)
-- tacticP =
--   choice
--     [ semicolonP,
--       atomicP
--     ]
