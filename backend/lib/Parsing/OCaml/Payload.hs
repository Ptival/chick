module Parsing.OCaml.Payload
  ( payload_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import OCaml.Parsing.ParseTree

payload_P :: Parser Structure -> Parser Payload
payload_P structure_P = choice
  [ PStr <$> structure_P
  ]
